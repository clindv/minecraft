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
        a (aget sight 0)
        b (aget sight 1)
        c (aget sight 2)
        aa (long (Math/floor a))
        bb (long (Math/floor b))
        cc (long (Math/floor c))
        draw (fn [x y]
               (let [^"[I" xx (int-array (map (comp (partial + (/ width 2)) (partial * width)) x))
                     ^"[I" yy (int-array (map (comp (partial + (/ height 2)) (partial * height)) y))]
                 (.drawPolygon graphics xx yy 4)))]
    (dotimes [z trunk-length]
      (let [z (if (< z cc) z (- (dec trunk-length) (- z cc)))
            z-trunk (* z z-trunk-offset)
            z-vertex (* z z-vertex-offset)]
        (dotimes [y trunk-length]
          (let [y (if (< y bb) y (- (dec trunk-length) (- y bb)))
                y-trunk (* y y-trunk-offset)
                y-vertex (* y y-vertex-offset)]
            (dotimes [x trunk-length]
              (let [x (if (< x aa) x (- (dec trunk-length) (- x aa)))
                    x-trunk (* x x-trunk-offset)
                    x-vertex (* x x-vertex-offset)
                    trunk-offset (+ z-trunk y-trunk x-trunk)
                    vertex-offset (+ z-vertex y-vertex x-vertex)]
                (if (aget trunk trunk-offset)
                  (let [x-ooo (aget vertex vertex-offset)
                        x-oox (aget vertex (+ vertex-offset x-vertex-offset))
                        x-oxo (aget vertex (+ vertex-offset y-vertex-offset))
                        x-oxx (aget vertex (+ vertex-offset y-vertex-offset x-vertex-offset))
                        x-xoo (aget vertex (+ vertex-offset z-vertex-offset))
                        x-xox (aget vertex (+ vertex-offset z-vertex-offset x-vertex-offset))
                        x-xxo (aget vertex (+ vertex-offset z-vertex-offset y-vertex-offset))
                        x-xxx (aget vertex (+ vertex-offset z-vertex-offset y-vertex-offset x-vertex-offset))
                        y-ooo (aget vertex (inc vertex-offset))
                        y-oox (aget vertex (inc (+ vertex-offset x-vertex-offset)))
                        y-oxo (aget vertex (inc (+ vertex-offset y-vertex-offset)))
                        y-oxx (aget vertex (inc (+ vertex-offset y-vertex-offset x-vertex-offset)))
                        y-xoo (aget vertex (inc (+ vertex-offset z-vertex-offset)))
                        y-xox (aget vertex (inc (+ vertex-offset z-vertex-offset x-vertex-offset)))
                        y-xxo (aget vertex (inc (+ vertex-offset z-vertex-offset y-vertex-offset)))
                        y-xxx (aget vertex (inc (+ vertex-offset z-vertex-offset y-vertex-offset x-vertex-offset)))]
                    (if (or (and (zero? x-ooo) (zero? y-ooo))
                            (and (zero? x-oox) (zero? y-oox))
                            (and (zero? x-oxo) (zero? y-oxo))
                            (and (zero? x-oxx) (zero? y-oxx))
                            (and (zero? x-xoo) (zero? y-xoo))
                            (and (zero? x-xox) (zero? y-xox))
                            (and (zero? x-xxo) (zero? y-xxo))
                            (and (zero? x-xxx) (zero? y-xxx))) nil
                        (do (if (or (<= z c) (zero? (aget trunk (+ trunk-offset 1)))) nil
                                (draw [x-ooo x-oxo x-oxx x-oox] [y-ooo y-oxo y-oxx y-oox]))
                            (if (or (<= y b) (zero? (aget trunk (+ trunk-offset 2)))) nil
                                (draw [x-ooo x-oox x-xox x-xoo] [y-ooo y-oox y-xox y-xoo]))
                            (if (or (<= x a) (zero? (aget trunk (+ trunk-offset 3)))) nil
                                (draw [x-ooo x-xoo x-xxo x-oxo] [y-ooo y-xoo y-xxo y-oxo]))
                            (if (or (>= (inc x) a) (zero? (aget trunk (+ trunk-offset 4)))) nil
                                (draw [x-oox x-oxx x-xxx x-xox] [y-oox y-oxx y-xxx y-xox]))
                            (if (or (>= (inc y) b) (zero? (aget trunk (+ trunk-offset 5)))) nil
                                (draw [x-oxo x-xxo x-xxx x-oxx] [y-oxo y-xxo y-xxx y-oxx]))
                            (if (or (>= (inc z) c) (zero? (aget trunk (+ trunk-offset 6)))) nil
                                (draw [x-xoo x-xox x-xxx x-xxo] [y-xoo y-xox y-xxx y-xxo]))))))))))))))
