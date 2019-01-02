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
(defn trans [line point]
  (let [ux (- (nth point 0) (nth line 0))
        uy (- (nth point 1) (nth line 1))
        uz (- (nth point 2) (nth line 2))
        vx (- (nth line 3) (nth line 0))
        vy (- (nth line 4) (nth line 1))
        vz (- (nth line 5) (nth line 2))
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
    [(> d 0) x y]))
(def cube [-40 -50 -60 -10 -20 -30])
(def sight [10 20 30 -40  -50 -60])
(map (partial trans sight) (partition 3 cube))
(def cube '[[0 1 2] [3 1 2] [0 4 2] [3 4 2]
            [0 1 5] [3 1 5] [0 4 5] [3 4 5]])
'[[0 1 2] [0 4 2] [3 4 2] [3 1 2]
  [0 1 5] [3 1 5] [3 4 5] [0 4 5]
  [0 1 2] [3 1 2] [3 1 5] [0 1 5]
  [0 4 2] [0 4 5] [3 4 5] [3 4 2]
  [0 1 2] [0 1 5] [0 4 5] [0 4 2]
  [3 1 2] [3 4 2] [3 4 5] [3 1 5]]
