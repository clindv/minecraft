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
(defn build-vertex [^doubles sight]
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
        yz (- (* vy vz))
        yy (+ (* vx vx) (* vz vz))
        yx (- (* vx vy))
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
(defn build-plain [^doubles sight]
  (let [z-trunk-offset (bit-shift-left (long (Math/pow trunk-length 2)) 3)
        y-trunk-offset (bit-shift-left trunk-length 3)
        x-trunk-offset (bit-shift-left 1 3)
        z-vertex-offset (bit-shift-left (long (Math/pow (inc trunk-length) 2)) 1)
        y-vertex-offset (bit-shift-left (inc trunk-length) 1)
        x-vertex-offset (bit-shift-left 1 1)
        a (long (Math/floor (aget sight 0)))
        b (long (Math/floor (aget sight 1)))
        c (long (Math/floor (aget sight 2)))
        screen-width 1000
        screen-height 1000
        buf (int-array 10)
        x-buf (int-array 4)
        y-buf (int-array 4)]
    (dotimes [z trunk-length]
      (let [z (if (< z c) z (- (dec trunk-length) (- z c)))
            z-trunk (* z z-trunk-offset)
            z-vertex (* z z-vertex-offset)]
        (dotimes [y trunk-length]
          (let [y (if (< y b) y (- (dec trunk-length) (- y b)))
                y-trunk (* y y-trunk-offset)
                y-vertex (* y y-vertex-offset)]
            (dotimes [x trunk-length]
              (let [x (if (< x a) x (- (dec trunk-length) (- x a)))
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
                    (if (or (<= z c) (zero? (aget trunk (+ trunk-offset 1)))) nil
                        (do (prn z y x "-" 1)
                            (double-array [x-ooo x-oxo x-xxo x-xoo])
                            (double-array [y-ooo y-oxo y-xxo y-xoo])))
                    (if (or (<= y b) (zero? (aget trunk (+ trunk-offset 2)))) nil
                        (do (prn z y x "-" 2)
                            (double-array [x-ooo x-xoo x-xox x-oox])
                            (double-array [y-ooo y-xoo y-xox y-oox])))
                    (if (or (<= x a) (zero? (aget trunk (+ trunk-offset 3)))) nil
                        (do (prn z y x "-" 3)
                            (double-array [x-ooo x-oxo x-oxx x-oox])
                            (double-array [y-ooo y-oxo y-oxx y-oox])))
                    (if (or (>= x a) (zero? (aget trunk (+ trunk-offset 4)))) nil
                        (do (prn z y x "-" 4)
                            (double-array [x-xoo x-xxo x-xxx x-xox])
                            (double-array [y-xoo y-xxo y-xxx y-xox])))
                    (if (or (>= y b) (zero? (aget trunk (+ trunk-offset 5)))) nil
                        (do (prn z y x "-" 5)
                            (double-array [x-oxo x-xxo x-xxx x-xox])
                            (double-array [y-xoo y-xxo y-xxx y-xox])))
                    (if (or (>= z c) (zero? (aget trunk (+ trunk-offset 6)))) nil
                        (do (prn z y x "-" 6)
                            (double-array [x-oox x-oxo x-xxx x-xox])
                            (double-array [y-oox y-oxo y-xxx y-xox])))))))))))))
