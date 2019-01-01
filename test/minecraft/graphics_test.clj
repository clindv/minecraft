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
    (testing "draw polygon"
      (let [strategy (.getBufferStrategy frame)
            bufx (int-array 6 (list 200 400 500 400 200 100))
            bufy (int-array 6 (list 100 100 300 500 500 300))]
        (dotimes [n 5]
          (let [graphics (.getDrawGraphics strategy)]
            (.clearRect graphics 0 0 800 600)
            (.drawPolygon graphics bufx bufy 6)
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
                        200 200 400 400
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
    (let [sight-line (->Line 1 2 3 4 -5 -6)
          point-a (->Point 4 -5 -6)
          point-b (->Point 4 -5 -7)]
      (is (= (trans sight-line point-a) [0.0 0.0]))
      (trans sight-line point-b))))
