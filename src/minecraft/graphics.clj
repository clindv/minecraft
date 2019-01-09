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
                (Math/sqrt (+ (* xx xx) (* xy xy) (* xz xz))))
             d)
        y (/ (/ (+ (* ux yx) (* uy yy) (* uz yz))
                (Math/sqrt (+ (* yx yx) (* yy yy) (* yz yz))))
             d)]
    [(> d 0) x y]))
(defn vertex-of-cube [cube]
  (map (partial map (partial nth cube))
       [[0 1 2] [3 1 2] [0 4 2] [3 4 2] [0 1 5] [3 1 5] [0 4 5] [3 4 5]]))
(defn convert [sight cube]
  (let [[t x y] (partition 8 (apply interleave
                                    (map (partial trans sight)
                                         (vertex-of-cube cube))))
        index '((0 2 3 1) (4 5 7 6) (0 1 5 4) (2 6 7 3) (0 4 6 2) (1 3 7 5))
        forward (map (comp not (partial apply (every-pred false?))
                           (partial map (partial nth t))) index)
        front (list (< (nth sight 2) (nth cube 2))
                    (> (nth sight 2) (nth cube 5))
                    (< (nth sight 1) (nth cube 1))
                    (> (nth sight 1) (nth cube 4))
                    (< (nth sight 0) (nth cube 0))
                    (> (nth sight 0) (nth cube 3)))
        visible (map (fn [x y] (and x y)) forward front)
        xbuf (map (partial map (partial nth x)) index)
        ybuf (map (partial map (partial nth y)) index)]
    (partition 3 (interleave visible xbuf ybuf))))
