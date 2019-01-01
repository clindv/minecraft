(ns minecraft.graphics
  (:import (java.awt Graphics
                     GraphicsConfiguration
                     GraphicsDevice
                     GraphicsEnvironment
                     image.VolatileImage
                     Toolkit)
           (javax.swing ImageIcon))
  (:gen-class))
(def ^{:tag Toolkit} toolkit-default (Toolkit/getDefaultToolkit))
(def ^{:tag GraphicsEnvironment :private true} graphics-environment
  (GraphicsEnvironment/getLocalGraphicsEnvironment))
(def ^{:tag GraphicsDevice :private true} screen-device-default
  (.getDefaultScreenDevice graphics-environment))
(def ^{:tag GraphicsConfiguration :private true} graphics-configuration
  (.getDefaultConfiguration screen-device-default))
;;(.setFullScreenWindow screen-device-default frame)
(def ^{:tag ImageIcon :private true} resource-image
  (ImageIcon. "./resources/resource.png"))
(def ^{:tag VolatileImage} resource-cache
  (.createCompatibleVolatileImage graphics-configuration
                                  (.getIconWidth resource-image)
                                  (.getIconHeight resource-image)))
(def ^{:tag Graphics} resource-cache-graphics
  (.getGraphics resource-cache))
(doto resource-cache-graphics
  ;;build cache
  (.drawImage (.getImage resource-image) 0 0 nil nil))
;;
(deftype Point [x y z])
(deftype Cube [a b c d e f])
(deftype Line [a b c d e f])
(defn trans [line point]
  (let [ux (- (.x point) (.a line))
        uy (- (.y point) (.b line))
        uz (- (.z point) (.c line))
        vx (- (.d line) (.a line))
        vy (- (.e line) (.b line))
        vz (- (.f line) (.c line))
        xx (- vz)
        xy 0
        xz vx
        yx (- (* vx vy))
        yy (+ (* vx vx) (* vz vz))
        yz (- (* vy vz))
        d (/ (+ (* ux vx) (* uy vy) (* uz vz))
             (Math/sqrt (+ (* vx vx) (* vy vy) (* vz vz))))
        x (/ (/ (+ (* ux xx) (* uy xy) (* uz xz))
                (Math/sqrt (+ (* xx xx) (* yy yy) (* xz xz))))
             d)
        y (/ (/ (+ (* ux yx) (* uy yy) (* uz yz))
                (Math/sqrt (+ (* yx yx) (* yy yy) (* yz yz))))
             d)]
    [x y]))
(def cube '[[a b c] [d b c] [a e c] [d e c]
           [a b f] [d b f] [a e f] [d e f]])
'[[a b c] [a e c] [d e c] [d b c]
 [a b f] [d b f] [d e f] [a e f]
 [a b c] [d b c] [d b f] [a b f]
 [a e c] [a e f] [d e f] [d e c]
 [a b c] [a b f] [a e f] [a e c]
 [d b c] [d e c] [d e f] [d b f]]
