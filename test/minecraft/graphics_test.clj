(ns minecraft.graphics-test
  (:import (java.awt Color
                     event.KeyEvent
                     event.KeyListener
                     event.WindowAdapter
                     event.WindowEvent
                     Frame
                     Graphics))
  (:require [clojure.test :refer :all]
            [minecraft.graphics :refer :all]
            [minecraft.control :as control]
            [minecraft.data :as data]))
(set! *warn-on-reflection* true)
(deftest geometry-test
  (testing "trunk"
    (let [^{:tag "[D"} sight (double-array [2 4 2 0.557988666 -0.162746694 0.813733471])]
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
      (build-vertex sight)
      (is (= (apply str (map (partial format "%1.1f") vertex))
             (str "0.0""0.0""0.0""0.0""0.0""0.0""0.0""0.0""-19.9""28.9"
                  "0.0""0.0""0.0""0.0""0.0""0.0""0.0""0.0""0.0""0.0"
                  "0.0""0.0""0.0""0.0""0.0""0.0""0.0""0.0""0.0""0.0"
                  "0.0""0.0""0.0""0.0""0.0""0.0""0.0""0.0""0.0""0.0"
                  "0.0""0.0""0.0""0.0""0.0""0.0""0.0""0.0""0.0""0.0"
                  "0.0""0.0""0.0""0.0""0.0""0.0""-3.5""10.1""-2.3""4.1"
                  "0.0""0.0""0.0""0.0""0.0""0.0""-6.0""12.9""-2.8""3.7"
                  "0.0""0.0""0.0""0.0""0.0""0.0""-19.9""28.9""-3.5""3.1"
                  "0.0""0.0""0.0""0.0""0.0""0.0""0.0""0.0""-4.8""2.0"
                  "0.0""0.0""0.0""0.0""0.0""0.0""0.0""0.0""-7.3""-0.2"
                  "0.0""0.0""8.9""43.4""0.0""6.1""-0.7""3.2""-0.9""2.1"
                  "0.0""0.0""0.0""0.0""0.0""6.1""-0.8""2.7""-1.0""1.7"
                  "0.0""0.0""0.0""0.0""0.0""6.1""-0.9""2.1""-1.1""1.2"
                  "0.0""0.0""0.0""0.0""0.0""6.1""-1.1""1.2""-1.3""0.6"
                  "0.0""0.0""0.0""0.0""0.0""0.0""-1.5""-0.2""-1.5""-0.2"
                  "6.4""11.5""1.5""4.3""0.4""2.6""-0.1""1.8""-0.4""1.4"
                  "11.9""16.2""1.9""3.9""0.4""2.2""-0.1""1.5""-0.4""1.1"
                  "95.3""87.0""2.4""3.3""0.5""1.6""-0.2""1.0""-0.5""0.7"
                  "0.0""0.0""3.3""2.3""0.6""0.9""-0.2""0.5""-0.5""0.3"
                  "0.0""0.0""5.4""-0.2""0.7""-0.2""-0.2""-0.2""-0.6""-0.2"
                  "2.4""3.3""1.1""2.2""0.5""1.6""0.1""1.3""-0.2""1.0"
                  "2.8""2.9""1.3""1.8""0.5""1.3""0.1""1.0""-0.2""0.8"
                  "3.3""2.3""1.4""1.3""0.6""0.9""0.1""0.6""-0.2""0.5"
                  "4.1""1.3""1.6""0.7""0.6""0.4""0.1""0.3""-0.2""0.2"
                  "5.4""-0.2""1.8""-0.2""0.7""-0.2""0.1""-0.2""-0.2""-0.2")))
      (beam [2 4 2] [-1.6 -2.4 0.4])
      (is (= (apply str trunk)
             (str "7123000071200000712000007120400071030500710005007100050071004500"
                  "0000000000000000000000000000000000000000000000000000000000000000"
                  "7023000070200000702000007020400070030560700005007000050070004500"
                  "0000000000000000000000000000000000000000000000000000000000000000"
                  "7023050070200000702000007020400000000000700305007000050070004500"
                  "0000000000000000000000000000000000000000000000000000000000000000"
                  "7023006070200060702000607020406071030560700005607000056070004560"
                  "0000000000000000000000000000000000000000000000000000000000000000")))
      (beam [2 4 2] [-1.6 -2.4 0.4])
      (= (apply str trunk)
             (str "7123000071200000712000007120400071030500710005007100050071004500"
                  "0000000000000000000000000000000000000000000000000000000000000000"
                  "7023000070200000702000007020406070030560700005007000050070004500"
                  "0000000000000000000000000000000000000000000000000000000000000000"
                  "7023050070200000702040000000000000000000700305007000050070204500"
                  "0000000000000000000000000000000000000000000000000000000000000000"
                  "7023006070200060702000607120406071030560700005607000056070004560"
                  "0000000000000000000000000000000000000000000000000000000000000000")))))
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
    (testing "draw image"
      (let [strategy (.getBufferStrategy frame)]
        (dotimes [n 1]
          (let [graphics (.getDrawGraphics strategy)]
            (.clearRect graphics 0 0 1200 800)
            (.drawImage graphics minecraft.graphics/resource-cache
                        0 0 64 64
                        0 0 64 64 nil nil)
            (.dispose graphics))
          (if (.contentsRestored strategy) "draw-pending" (.show strategy))
          (if (.contentsLost strategy) "cache-regeneration")
          (.sync toolkit-default)
          (Thread/sleep 50))))
    (testing "draw trunk"
      (let [strategy (.getBufferStrategy frame)]
        (data/build-camera)
        (control/unleash data/camera)
        (control/leash data/camera data/wasd-fly data/arrows-pitch-yaw data/space-dig)
        (build-trunk 2)
        (build-bottom-layers 1 7)
        (aset-byte trunk (+ 256 128 32) 8)
        (build-surface)
        (dotimes [n 8000]
          (let [^{:tag "Graphics"} graphics (.getDrawGraphics strategy)
                ^{:tag "[D"} sight (double-array (concat @(minecraft.data/camera :position)
                                                         @(minecraft.data/camera :orientation)))
                width 1200
                height 800]
            (.clearRect graphics 0 0 1200 800)
            (.clipRect graphics 0 0 1200 800)
            (.drawPolygon graphics (int-array [0 width width 0]) (int-array [0 0 height height]) 4)
            (build-vertex sight)
            (draw-trunk sight graphics width height)
            (.setColor graphics Color/RED)
            (.drawLine graphics (- 600 9) 400 (+ 600 9) 400)
            (.drawLine graphics 600 (- 400 9) 600 (+ 400 9))
            (.setColor graphics Color/BLACK)
            (data/refresh data/camera)
            (.dispose graphics))
          (if (.contentsRestored strategy) "draw-pending" (.show strategy))
          (if (.contentsLost strategy) "cache-regeneration")
          (.sync toolkit-default)
          (Thread/sleep 1))
        (control/unleash data/camera)))
    (testing "dispose"
      (doto frame
        (.setVisible false)
        (.dispose)))))
