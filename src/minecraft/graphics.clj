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
(defn build-trunk [depth]
  (def trunk-depth depth)
  (def trunk-length (bit-shift-left 1 trunk-depth))
  (def ^{:tag "[B"} trunk (byte-array (bit-shift-left (long (Math/pow trunk-length 3)) 3)))
  (def ^{:tag "[D"} vertex (double-array (bit-shift-left (long (Math/pow (inc trunk-length) 3)) 1))))
(defn build-bottom-layers [^long n ^long content]
  (let [z-offset (bit-shift-left (long (Math/pow trunk-length 2)) 3)
        y-offset (bit-shift-left trunk-length 3)
        x-offset (bit-shift-left 1 3)]
    (dotimes [z trunk-length]
      (dotimes [y n]
        (dotimes [x trunk-length]
          (let [offset (+ (* z z-offset) (* y y-offset) (* x x-offset))]
            (aset-byte trunk offset content)))))))
(defn build-surface []
  (let [z-offset (bit-shift-left (long (Math/pow trunk-length 2)) 3)
        y-offset (bit-shift-left trunk-length 3)
        x-offset (bit-shift-left 1 3)]
    (dotimes [z trunk-length]
      (dotimes [y trunk-length]
        (dotimes [x trunk-length]
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
                    (if (or (= x (dec trunk-length))
                            (zero? (aget trunk (+ offset x-offset))))
                      (aset-byte trunk (+ offset 4) 4)
                      (aset-byte trunk (+ offset 4) 0))
                    (if (or (= y (dec trunk-length))
                            (zero? (aget trunk (+ offset y-offset))))
                      (aset-byte trunk (+ offset 5) 5)
                      (aset-byte trunk (+ offset 5) 0))
                    (if (or (= z (dec trunk-length))
                            (zero? (aget trunk (+ offset z-offset))))
                      (aset-byte trunk (+ offset 6) 6)
                      (aset-byte trunk (+ offset 6) 0))))))))))
(defn build-vertex [^{:tag "[D"} sight]
  (let [z-offset (bit-shift-left (long (Math/pow (inc trunk-length) 2)) 1)
        y-offset (bit-shift-left (inc trunk-length) 1)
        x-offset (bit-shift-left 1 1)
        vz (- (aget sight 5) (aget sight 2))
        vy (- (aget sight 4) (aget sight 1))
        vx (- (aget sight 3) (aget sight 0))
        v2v (Math/sqrt (+ (Math/pow vx 2) (Math/pow vy 2) (Math/pow vz 2)))
        xz vx
        xy 0
        xx (- vz)
        x2x (Math/sqrt (+ (Math/pow xx 2) (Math/pow xy 2) (Math/pow xz 2)))
        yz (* vy vz)
        yy (- 0 (* vx vx) (* vz vz))
        yx (* vx vy)
        y2y (Math/sqrt (+ (Math/pow yx 2) (Math/pow yy 2) (Math/pow yz 2)))]
    (dotimes [z (inc trunk-length)]
      (dotimes [y (inc trunk-length)]
        (dotimes [x (inc trunk-length)]
          (let [offset (+ (* z z-offset) (* y y-offset) (* x x-offset))
                uz (- z (aget sight 2))
                uy (- y (aget sight 1))
                ux (- x (aget sight 0))
                d (/ (+ (* ux vx) (* uy vy) (* uz vz)) v2v)]
            (if (pos? d)
              (let [x (/ (/ (+ (* ux xx) (* uy xy) (* uz xz)) x2x) d)
                    y (/ (/ (+ (* ux yx) (* uy yy) (* uz yz)) y2y) d)]
                (aset-double vertex offset x)
                (aset-double vertex (inc offset) y))
              (do (aset-double vertex offset 0)
                  (aset-double vertex (inc offset) 0)))))))))
(defn draw-trunk [^{:tag "[D"} sight ^{:tag "Graphics"} graphics width height]
  (let [z-trunk-offset (bit-shift-left (long (Math/pow trunk-length 2)) 3)
        y-trunk-offset (bit-shift-left trunk-length 3)
        x-trunk-offset (bit-shift-left 1 3)
        z-vertex-offset (bit-shift-left (long (Math/pow (inc trunk-length) 2)) 1)
        y-vertex-offset (bit-shift-left (inc trunk-length) 1)
        x-vertex-offset (bit-shift-left 1 1)
        order (fn [index sight-index] (if (< index sight-index) index (- (+ sight-index trunk-length) index 1)))
        draw (fn [x y] (.drawPolygon graphics (int-array (map (comp (partial + (/ width 2)) (partial * width)) x))
                                     (int-array (map (comp (partial + (/ height 2)) (partial * height)) y)) 4))]
    (dotimes [z trunk-length]
      (let [z (order z (long (Math/floor (aget sight 2))))]
        (dotimes [y trunk-length]
          (let [y (order y (long (Math/floor (aget sight 1))))]
            (dotimes [x trunk-length]
              (let [x (order x (long (Math/floor (aget sight 0))))
                    trunk-offset (+ (* z z-trunk-offset) (* y y-trunk-offset) (* x x-trunk-offset))
                    vertex-offset (+ (* z z-vertex-offset) (* y y-vertex-offset) (* x x-vertex-offset))]
                (if (zero? (aget trunk trunk-offset)) nil
                    (let [[x0 x1 x2 x3 x4 x5 x6 x7 y0 y1 y2 y3 y4 y5 y6 y7]
                          (for [d [0 1] c [0 z-vertex-offset] b [0 y-vertex-offset] a [0 x-vertex-offset]]
                            (aget vertex (+ vertex-offset d c b a)))]
                      (if (or (and (zero? x0) (zero? y0))
                              (and (zero? x1) (zero? y1))
                              (and (zero? x2) (zero? y2))
                              (and (zero? x3) (zero? y3))
                              (and (zero? x4) (zero? y4))
                              (and (zero? x5) (zero? y5))
                              (and (zero? x6) (zero? y6))
                              (and (zero? x7) (zero? y7))) nil
                          (do (if (or (<= z (aget sight 2)) (zero? (aget trunk (+ trunk-offset 1)))) nil
                                  (draw [x0 x2 x3 x1] [y0 y2 y3 y1]))
                              (if (or (<= y (aget sight 1)) (zero? (aget trunk (+ trunk-offset 2)))) nil
                                  (draw [x0 x1 x5 x4] [y0 y1 y5 y4]))
                              (if (or (<= x (aget sight 0)) (zero? (aget trunk (+ trunk-offset 3)))) nil
                                  (draw [x0 x4 x6 x2] [y0 y4 y6 y2]))
                              (if (or (>= (inc x) (aget sight 0)) (zero? (aget trunk (+ trunk-offset 4)))) nil
                                  (draw [x1 x3 x7 x5] [y1 y3 y7 y5]))
                              (if (or (>= (inc y) (aget sight 1)) (zero? (aget trunk (+ trunk-offset 5)))) nil
                                  (draw [x2 x6 x7 x3] [y2 y6 y7 y3]))
                              (if (or (>= (inc z) (aget sight 2)) (zero? (aget trunk (+ trunk-offset 6)))) nil
                                  (draw [x4 x5 x7 x6] [y4 y5 y7 y6]))))))))))))))
