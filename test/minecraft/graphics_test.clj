(ns minecraft.graphics-test
  (:import (java.awt event.KeyEvent
                     event.KeyListener
                     event.WindowAdapter
                     event.WindowEvent
                     Frame))
  (:require [clojure.test :refer :all]
            [minecraft.graphics :refer :all]
            [minecraft.control :as control]))
(deftest frame-test
  (let [^{:tag Frame} frame (proxy [Frame] ["minecraft"])]
    (testing "frame"
      (doto frame
        (.addWindowListener (proxy [WindowAdapter] []
                              (windowClosing [^WindowEvent e]
                                (doto (.getWindow e)
                                  (.setVisible false)
                                  (.dispose)))))
        (.addKeyListener (proxy [KeyListener] []
                           (keyTyped [^KeyEvent e])
                           (keyReleased [^KeyEvent e]
                             (control/stroke :release (.getKeyCode e)))
                           (keyPressed [^KeyEvent e]
                             (control/stroke :press (.getKeyCode e)))))
        (.setFocusTraversalKeysEnabled false)
        (.setExtendedState Frame/MAXIMIZED_BOTH)
        (.setIgnoreRepaint true)
        (.setVisible true)
        (.createBufferStrategy 2)))
    (testing "draw cube"
      (let [strategy (.getBufferStrategy frame)
            sight [1 2 3 4 5 -6]
            cube [4 5 -6 5 6 -5]
            plains (convert sight cube)]
        (dotimes [n 5]
          (let [graphics (.getDrawGraphics strategy)]
            (.clearRect graphics 0 0 800 600)
            (doseq [[t bufx bufy] plains]
              (if t (.drawPolygon graphics
                                  (int-array (map (comp (partial * 1000)
                                             (partial + 0.1))
                                       bufx))
                                  (int-array (map (comp (partial * 1000)
                                             (partial + 0.1))
                                       bufy))
                                  4)))
            (.dispose graphics))
          (if (.contentsRestored strategy) "draw-pending" (.show strategy))
          (if (.contentsLost strategy) "cache-regeneration")
          (.sync toolkit-default)
          (Thread/sleep 100))))
    (testing "draw image"
      (let [strategy (.getBufferStrategy frame)]
        (dotimes [n 10]
          (let [graphics (.getDrawGraphics strategy)]
            (.drawImage graphics minecraft.graphics/resource-cache
                        0 0 64 64
                        0 0 64 64 nil nil)
            (.dispose graphics))
          (if (.contentsRestored strategy) "draw-pending" (.show strategy))
          (if (.contentsLost strategy) "cache-regeneration")
          (.sync toolkit-default)
          (Thread/sleep 100))))
    (testing "dispose"
      (doto frame
        (.setVisible false)
        (.dispose)))))
(deftest geometry-test
  (testing "point-trans"
    (let [sight-line [1 2 3 4 -5 -6]
          point-a [4 -5 -6]
          point-b [4 -5 -7]]
      (is (= (trans sight-line point-a) [true 0.0 0.0]))
      (trans sight-line point-b)))
  (testing "cube-trans"
    (let [sight [10 20 30 -40  -50 -60]
          cube [-40 -50 -60 -10 -20 -30]]
      (convert sight cube)))
  (testing "trunk"
    (let [side-depth 2
          side-length (bit-shift-left 1 side-depth)
          trunk (byte-array (bit-shift-left (long (Math/pow side-length 3)) 3))
          z-offset (bit-shift-left (long (Math/pow side-length 2)) 3)
          y-offset (bit-shift-left side-length 3)
          x-offset (bit-shift-left 1 3)]
      (dotimes [z side-length]
        (dotimes [y 2]
          (dotimes [x side-length]
            (aset-byte trunk (+ (* z z-offset) (* y y-offset) (* x x-offset)) 7))))
      (pr "init:")
      (dotimes [n (alength trunk)] (if (zero? (Math/floorMod n 64)) (prn)) (pr (aget trunk n)))
      (dotimes [z side-length]
        (dotimes [y side-length]
          (dotimes [x side-length]
            (let [offset (+ (* z z-offset) (* y y-offset) (* x x-offset))]
              (if (zero? (aget trunk offset)) nil
                  (do (if (or (zero? z)
                              (zero? (aget trunk (- offset z-offset))))
                        (aset-byte trunk (+ offset 1) 1)
                        (aset-byte trunk (+ offset 1) 0))
                      (if (or (zero? y)
                              (zero? (aget trunk (- offset y-offset))))
                        (aset-byte trunk (+ offset 2) 2)
                        (aset-byte trunk (+ offset 2) 0))
                      (if (or (zero? x)
                              (zero? (aget trunk (- offset x-offset))))
                        (aset-byte trunk (+ offset 3) 3)
                        (aset-byte trunk (+ offset 3) 0))
                      (if (or (= x (dec side-length))
                              (zero? (aget trunk (+ offset x-offset))))
                        (aset-byte trunk (+ offset 4) 4)
                        (aset-byte trunk (+ offset 4) 0))
                      (if (or (= y (dec side-length))
                              (zero? (aget trunk (+ offset y-offset))))
                        (aset-byte trunk (+ offset 5) 5)
                        (aset-byte trunk (+ offset 5) 0))
                      (if (or (= z (dec side-length))
                              (zero? (aget trunk (+ offset z-offset))))
                        (aset-byte trunk (+ offset 6) 6)
                        (aset-byte trunk (+ offset 6) 0))))))))
      (prn)
      (pr "build:")
      (dotimes [n (alength trunk)] (if (zero? (Math/floorMod n 64)) (prn)) (pr (aget trunk n)))
      (let [sight (double-array [2 2 2 3.3 4.4 5.5])
            vertex (double-array (bit-shift-left (long (Math/pow (inc side-length) 3)) 1))
            z-offset (bit-shift-left (long (Math/pow (inc side-length) 2)) 1)
            y-offset (bit-shift-left (inc side-length) 1)
            x-offset (bit-shift-left 1 1)
            vz (- (aget sight 5) (aget sight 2))
            vy (- (aget sight 4) (aget sight 1))
            vx (- (aget sight 3) (aget sight 0))
            v2v (Math/sqrt (+ (Math/pow vx 2) (Math/pow vy 2) (Math/pow vz 2)))
            xz vx
            xy 0
            xx (- vz)
            x2x (Math/sqrt (+ (Math/pow xx 2) (Math/pow xy 2) (Math/pow xz 2)))
            yz (- (* vy vz))
            yy (+ (* vx vx) (* vz vz))
            yx (- (* vx vy))
            y2y (Math/sqrt (+ (Math/pow yx 2) (Math/pow yy 2) (Math/pow yz 2)))]
        (prn)
        (pr "init:")
        (dotimes [n (alength vertex)] (if (zero? (Math/floorMod n 2)) (prn)) (pr (aget vertex n)))
        (dotimes [z (inc side-length)]
          (dotimes [y (inc side-length)]
            (dotimes [x (inc side-length)]
              (let [offset (+ (* z z-offset) (* y y-offset) (* x x-offset))
                    uz (- z (aget sight 2))
                    uy (- y (aget sight 1))
                    ux (- x (aget sight 0))
                    d (/ (+ (* ux vx) (* uy vy) (* uz vz)) v2v)
                    x (/ (/ (+ (* ux xx) (* uy xy) (* uz xz)) x2x) d)
                    y (/ (/ (+ (* ux yx) (* uy yy) (* uz yz)) y2y) d)]
                (if (pos? d)
                  (do (aset-double vertex offset x)
                      (aset-double vertex (inc offset) y))
                  (do (aset-double vertex offset 0)
                      (aset-double vertex (inc offset) 0)))))))
        (prn)
        (pr "build:")
        (dotimes [n (alength vertex)] (if (zero? (Math/floorMod n 2)) (prn)) (pr (aget vertex n)))))))
'([[x y z] [x (inc y) z] [(inc x) (inc y) z] [(inc x) y z]]
[[x y z] [(inc x) y z] [(inc x) y (inc z)] [x y (inc z)]]
[[x y z] [x (inc y) z] [x (inc y) (inc z)] [x y (inc z)]]
[[(inc x) y z] [(inc x) (inc y) z] [(inc x) (inc y) (inc z)] [(inc x) y (inc z)]]
[[x (inc y) z] [(inc x) (inc y) z] [(inc x) (inc y) (inc z)] [x (inc y) (inc z)]]
[[x y (inc z)] [x (inc y) z] [(inc x) (inc y) (inc z)] [(inc x) y (inc z)]])
