(ns minecraft.core
  (:import (java.awt Color
                     event.KeyEvent
                     event.KeyListener
                     event.WindowAdapter
                     event.WindowEvent
                     Frame
                     Graphics))
  (:require [minecraft.control :as control]
            [minecraft.data :as data]
            [minecraft.graphics :as graphics]
            [minecraft.time :as time])
  (:gen-class))
(set! *warn-on-reflection* true)
(minecraft.time/tick :run)
(def ^{:tag Frame} frame (proxy [Frame] ["minecraft"]))
(doto frame
  (.addWindowListener (proxy [WindowAdapter] []
                        (windowClosing [^{:tag WindowEvent} e]
                          (doto (.getWindow e)
                            (.setVisible false)
                            (.dispose)))))
  (.addKeyListener (proxy [KeyListener] []
                     (keyTyped [^{:tag KeyEvent} e])
                     (keyReleased [^{:tag KeyEvent} e]
                       (control/stroke :release (.getKeyCode e)))
                     (keyPressed [^{:tag KeyEvent} e]
                       (control/stroke :press (.getKeyCode e)))))
  (.setFocusTraversalKeysEnabled false)
  (.setExtendedState Frame/MAXIMIZED_BOTH)
  (.setBackground Color/GRAY)
  (.setIgnoreRepaint true)
  (.setVisible true)
  (.createBufferStrategy 2))
(defn -main
  [& args]
  (let [strategy (.getBufferStrategy frame)]
    (data/build-camera [3 7 2])
    (control/unleash data/camera)
    (control/leash data/camera data/wasd-fly data/arrows-pitch-yaw data/space-dig data/enter-build)
    (graphics/build-trunk 3)
    (graphics/build-bottom-layers 5 7)
    (graphics/build-surface)
    (dotimes [n 10000]
      (let [^{:tag "Graphics"} g (.getDrawGraphics strategy)
            ^{:tag "[D"} sight (double-array (concat @(data/camera :position) @(data/camera :orientation)))
            width (.getWidth frame)
            height (.getHeight frame)]
        (.clearRect g 0 0 width height)
        (.clipRect g 0 0 width height)
        (.drawPolygon g (int-array [0 width width 0]) (int-array [0 0 height height]) 4)
        (graphics/build-vertex sight)
        (graphics/draw-trunk sight g width height)
        (data/refresh data/camera)
        (.setColor g Color/RED)
        (.drawLine g (- (/ width 2) 15) (/ height 2) (+ (/ width 2) 15) (/ height 2))
        (.drawLine g (/ width 2) (- (/ height 2) 15) (/ width 2) (+ (/ height 2) 15))
        (.setColor g Color/BLACK)
        (.dispose g))
      (if (.contentsRestored strategy) "draw-pending" (.show strategy))
      (if (.contentsLost strategy) "cache-regeneration")
      (.sync graphics/toolkit-default)
      (Thread/sleep 1))
    (control/unleash data/camera)))
