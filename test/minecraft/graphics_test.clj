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
    (build-trunk 2)
    (is (= (apply str trunk)
           (str "0000000000000000000000000000000000000000000000000000000000000000"
                "0000000000000000000000000000000000000000000000000000000000000000"
                "0000000000000000000000000000000000000000000000000000000000000000"
                "0000000000000000000000000000000000000000000000000000000000000000"
                "0000000000000000000000000000000000000000000000000000000000000000"
                "0000000000000000000000000000000000000000000000000000000000000000"
                "0000000000000000000000000000000000000000000000000000000000000000"
                "0000000000000000000000000000000000000000000000000000000000000000")))
    (build-bottom-layers 2 7)
    (is (= (apply str trunk)
           (str "7000000070000000700000007000000070000000700000007000000070000000"
                "0000000000000000000000000000000000000000000000000000000000000000"
                "7000000070000000700000007000000070000000700000007000000070000000"
                "0000000000000000000000000000000000000000000000000000000000000000"
                "7000000070000000700000007000000070000000700000007000000070000000"
                "0000000000000000000000000000000000000000000000000000000000000000"
                "7000000070000000700000007000000070000000700000007000000070000000"
                "0000000000000000000000000000000000000000000000000000000000000000")))
    (build-surface)
    (is (= (apply str trunk)
           (str "7123000071200000712000007120400071030500710005007100050071004500"
                "0000000000000000000000000000000000000000000000000000000000000000"
                "7023000070200000702000007020400070030500700005007000050070004500"
                "0000000000000000000000000000000000000000000000000000000000000000"
                "7023000070200000702000007020400070030500700005007000050070004500"
                "0000000000000000000000000000000000000000000000000000000000000000"
                "7023006070200060702000607020406070030560700005607000056070004560"
                "0000000000000000000000000000000000000000000000000000000000000000")))
    (is (= (apply str (map (partial format "%1.1f") vertex))
           (apply str (repeat 250 "0.0"))))
    (build-vertex (double-array [2 4 2 4.4 3.3 5.5]))
    (is (= (apply str (map (partial format "%1.1f") vertex))
           (str "0.0""0.0""0.0""0.0""0.0""0.0""0.0""0.0""-19.9""-28.9"
                "0.0""0.0""0.0""0.0""0.0""0.0""0.0""0.0""0.0""0.0"
                "0.0""0.0""0.0""0.0""0.0""0.0""0.0""0.0""0.0""0.0"
                "0.0""0.0""0.0""0.0""0.0""0.0""0.0""0.0""0.0""0.0"
                "0.0""0.0""0.0""0.0""0.0""0.0""0.0""0.0""0.0""0.0"
                "0.0""0.0""0.0""0.0""0.0""0.0""-3.5""-10.1""-2.3""-4.1"
                "0.0""0.0""0.0""0.0""0.0""0.0""-6.0""-12.9""-2.8""-3.7"
                "0.0""0.0""0.0""0.0""0.0""0.0""-19.9""-28.9""-3.5""-3.1"
                "0.0""0.0""0.0""0.0""0.0""0.0""0.0""0.0""-4.8""-2.0"
                "0.0""0.0""0.0""0.0""0.0""0.0""0.0""0.0""-7.3""0.2"
                "0.0""0.0""8.9""-43.4""0.0""-6.1""-0.7""-3.2""-0.9""-2.1"
                "0.0""0.0""0.0""0.0""0.0""-6.1""-0.8""-2.7""-1.0""-1.7"
                "0.0""0.0""0.0""0.0""0.0""-6.1""-0.9""-2.1""-1.1""-1.2"
                "0.0""0.0""0.0""0.0""0.0""-6.1""-1.1""-1.2""-1.3""-0.6"
                "0.0""0.0""0.0""0.0""0.0""0.0""-1.5""0.2""-1.5""0.2"
                "6.4""-11.5""1.5""-4.3""0.4""-2.6""-0.1""-1.8""-0.4""-1.4"
                "11.9""-16.2""1.9""-3.9""0.4""-2.2""-0.1""-1.5""-0.4""-1.1"
                "95.3""-87.0""2.4""-3.3""0.5""-1.6""-0.2""-1.0""-0.5""-0.7"
                "0.0""0.0""3.3""-2.3""0.6""-0.9""-0.2""-0.5""-0.5""-0.3"
                "0.0""0.0""5.4""0.2""0.7""0.2""-0.2""0.2""-0.6""0.2"
                "2.4""-3.3""1.1""-2.2""0.5""-1.6""0.1""-1.3""-0.2""-1.0"
                "2.8""-2.9""1.3""-1.8""0.5""-1.3""0.1""-1.0""-0.2""-0.8"
                "3.3""-2.3""1.4""-1.3""0.6""-0.9""0.1""-0.6""-0.2""-0.5"
                "4.1""-1.3""1.6""-0.7""0.6""-0.4""0.1""-0.3""-0.2""-0.2"
                "5.4""0.2""1.8""0.2""0.7""0.2""0.1""0.2""-0.2""0.2")))
    (let [sight (double-array [2 4 2 4.4 3.3 5.5])]
      (build-plain sight)
      (prn "---")
      (build-trunk 2)
      (build-bottom-layers 3 7)
      (aset-byte trunk (+ 256 128 64 32) 8)
      (build-surface)
      (build-vertex sight)
      (build-plain sight))
    ))
