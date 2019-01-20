(ns minecraft.graphics
  (:import (java.awt Color
                     Graphics
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
        vz (aget sight 5)
        vy (aget sight 4)
        vx (aget sight 3)
        v2v (Math/sqrt (+ (Math/pow vx 2) (Math/pow vy 2) (Math/pow vz 2)))
        x2x (Math/sqrt (+ (Math/pow vz 2) (Math/pow vx 2)))
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
              (do (aset-double vertex offset (/ (/ (- (* uz vx) (* ux vz)) x2x) d))
                  (aset-double vertex (inc offset) (/ (/ (+ (* ux yx) (* uy yy) (* uz yz)) y2y) d)))
              (do (aset-double vertex offset 0)
                  (aset-double vertex (inc offset) 0)))))))))
(defn draw-trunk [^{:tag "[D"} sight ^{:tag "Graphics"} graphics width height]
  (let [z-trunk-offset (bit-shift-left (long (Math/pow trunk-length 2)) 3)
        y-trunk-offset (bit-shift-left trunk-length 3)
        x-trunk-offset (bit-shift-left 1 3)
        z-vertex-offset (bit-shift-left (long (Math/pow (inc trunk-length) 2)) 1)
        y-vertex-offset (bit-shift-left (inc trunk-length) 1)
        x-vertex-offset (bit-shift-left 1 1)
        [xx yy zz] (map #(long (Math/floor (aget sight %))) (range 3))
        draw (fn [x y]
               (.setColor graphics Color/WHITE)
               (.fillPolygon graphics (int-array (map (comp (partial + (/ width 2)) (partial * width)) x))
                             (int-array (map (comp (partial + (/ height 2)) (partial * height)) y)) 4)
               (.setColor graphics Color/BLACK)
               (.drawPolygon graphics (int-array (map (comp (partial + (/ width 2)) (partial * width)) x))
                             (int-array (map (comp (partial + (/ height 2)) (partial * height)) y)) 4))]
    (doseq [z (concat (range (min trunk-length zz)) (reverse (range (max zz 0) trunk-length)))
            y (concat (range (min trunk-length yy)) (reverse (range (max yy 0) trunk-length)))
            x (concat (range (min trunk-length xx)) (reverse (range (max xx 0) trunk-length)))]
      (let [trunk-offset (+ (* z z-trunk-offset) (* y y-trunk-offset) (* x x-trunk-offset))
            vertex-offset (+ (* z z-vertex-offset) (* y y-vertex-offset) (* x x-vertex-offset))]
        (if (zero? (aget trunk trunk-offset)) nil
            (let [v (for [d [0 1] c [0 z-vertex-offset] b [0 y-vertex-offset] a [0 x-vertex-offset]]
                      (aget vertex (+ vertex-offset d c b a)))]
              (if (or (and (zero? (nth v 0)) (zero? (nth v 8)))
                      (and (zero? (nth v 1)) (zero? (nth v 9)))
                      (and (zero? (nth v 2)) (zero? (nth v 10)))
                      (and (zero? (nth v 3)) (zero? (nth v 11)))
                      (and (zero? (nth v 4)) (zero? (nth v 12)))
                      (and (zero? (nth v 5)) (zero? (nth v 13)))
                      (and (zero? (nth v 6)) (zero? (nth v 14)))
                      (and (zero? (nth v 7)) (zero? (nth v 15)))) nil
                  (do (if (or (<= z (aget sight 2)) (zero? (aget trunk (+ trunk-offset 1)))) nil
                          (draw (map (partial nth v) [0 2 3 1]) (map (partial nth v) [8 10 11 9])))
                      (if (or (<= y (aget sight 1)) (zero? (aget trunk (+ trunk-offset 2)))) nil
                          (draw (map (partial nth v) [0 1 5 4]) (map (partial nth v) [8 9 13 12])))
                      (if (or (<= x (aget sight 0)) (zero? (aget trunk (+ trunk-offset 3)))) nil
                          (draw (map (partial nth v) [0 4 6 2]) (map (partial nth v) [8 12 14 10])))
                      (if (or (>= (inc x) (aget sight 0)) (zero? (aget trunk (+ trunk-offset 4)))) nil
                          (draw (map (partial nth v) [1 3 7 5]) (map (partial nth v) [9 11 15 13])))
                      (if (or (>= (inc y) (aget sight 1)) (zero? (aget trunk (+ trunk-offset 5)))) nil
                          (draw (map (partial nth v) [2 6 7 3]) (map (partial nth v) [10 14 15 11])))
                      (if (or (>= (inc z) (aget sight 2)) (zero? (aget trunk (+ trunk-offset 6)))) nil
                          (draw (map (partial nth v) [4 5 7 6]) (map (partial nth v) [12 13 15 14])))))))))))
