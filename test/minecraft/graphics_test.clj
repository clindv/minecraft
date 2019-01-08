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
                                  (int-array (map (comp (partial * 5000)
                                             (partial + 0.1))
                                       bufx))
                                  (int-array (map (comp (partial * 800)
                                             (partial + 0.1))
                                       bufy))
                                  4)))
            (.dispose graphics))
          (if (.contentsRestored strategy) "draw-pending" (.show strategy))
          (if (.contentsLost strategy) "cache-regeneration")
          (.sync toolkit-default)
          (Thread/sleep 100))))
    (testing "draw image"
      (let [strategy (.getBufferStrategy frame)
            bufx (int-array 6 (list 200 400 500 400 200 100))
            bufy (int-array 6 (list 100 100 300 500 500 300))]
        (dotimes [n 10]
          (let [graphics (.getDrawGraphics strategy)]
            (.drawImage graphics minecraft.graphics/resource-cache
                        0 0 128 128
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
      (dotimes [c side-length]
        (dotimes [b 2]
          (dotimes [a side-length]
            (aset-byte trunk (+ (* c z-offset) (* b y-offset) (* a x-offset)) 7))))
      (pr "init:")
      (dotimes [n (alength trunk)] (if (zero? (Math/floorMod n 64)) (prn)) (pr (aget trunk n)))
      (dotimes [c side-length]
        (dotimes [b side-length]
          (dotimes [a side-length]
            (let [offset (+ (* c z-offset) (* b y-offset) (* a x-offset))]
              (if (zero? (aget trunk offset)) nil
                  (do (if (or (zero? c)
                              (zero? (aget trunk (- offset z-offset))))
                        (aset-byte trunk (+ offset 1) 1)
                        (aset-byte trunk (+ offset 1) 0))
                      (if (or (zero? b)
                              (zero? (aget trunk (- offset y-offset))))
                        (aset-byte trunk (+ offset 2) 2)
                        (aset-byte trunk (+ offset 2) 0))
                      (if (or (zero? a)
                              (zero? (aget trunk (- offset x-offset))))
                        (aset-byte trunk (+ offset 3) 3)
                        (aset-byte trunk (+ offset 3) 0))
                      (if (or (= a (dec side-length))
                              (zero? (aget trunk (+ offset x-offset))))
                        (aset-byte trunk (+ offset 4) 4)
                        (aset-byte trunk (+ offset 4) 0))
                      (if (or (= b (dec side-length))
                              (zero? (aget trunk (+ offset y-offset))))
                        (aset-byte trunk (+ offset 5) 5)
                        (aset-byte trunk (+ offset 5) 0))
                      (if (or (= c (dec side-length))
                              (zero? (aget trunk (+ offset z-offset))))
                        (aset-byte trunk (+ offset 6) 6)
                        (aset-byte trunk (+ offset 6) 0))))))))
      (prn)
      (pr "build:")
      (dotimes [n (alength trunk)] (if (zero? (Math/floorMod n 64)) (prn)) (pr (aget trunk n))))))
