(ns minecraft.data
  (:require (minecraft time
                       control))
  (:gen-class))
(defn normalize [v]
  (map #(/ % (Math/sqrt (apply + (map (fn [a] (Math/pow a 2)) v)))) v))
(defn physics []
  {:position (atom [3 5 2])
   :velocity (atom [0 0 0])
   :acceleration (atom [0 0 0])
   :orientation (atom (normalize [-1.6 -2.4 0.4]))
   :angular-velocity (atom [0 0 0])
   :angular-acceleration (atom [0 0 0])})
(def camera
  (conj (physics)
        [:id (atom "camera")]
        [:tick (atom (minecraft.time/tick :now))]))
(defn refresh [o]
  (let [past-tick @(o :tick)
        delta-tick (- (reset! (o :tick) (minecraft.time/tick :now)) past-tick)]
    (swap! (o :velocity) (partial map + (map (partial * (/ delta-tick 1e3)) @(o :acceleration))))
    (swap! (o :position) (partial map + (map (partial * (/ delta-tick 1e2)) @(o :velocity))))
    (swap! (o :angular-velocity) (partial map + (map (partial * (/ delta-tick 1e3)) @(o :angular-acceleration))))
    (reset! (o :orientation) (let [cos-theta (Math/cos (/ delta-tick 1e3))
                                   sin-theta (Math/sin (/ delta-tick 1e3))
                                   [x y z] @(o :angular-velocity)]
                               (map (comp (partial apply +) (partial map * @(o :orientation)))
                                    [[(+ cos-theta (* (Math/pow x 2) (- 1 cos-theta)))
                                      (- (* x y (- 1 cos-theta)) (* z sin-theta))
                                      (+ (* x z (- 1 cos-theta)) (* y sin-theta))]
                                     [(+ (* x y (- 1 cos-theta)) (* z sin-theta))
                                      (+ cos-theta (* (Math/pow y 2) (- 1 cos-theta)))
                                      (- (* y z (- 1 cos-theta)) (* x sin-theta))]
                                     [(- (* x z (- 1 cos-theta)) (* y sin-theta))
                                      (+ (* y z (- 1 cos-theta)) (* x sin-theta))
                                      (+ cos-theta (* (Math/pow z 2) (- 1 cos-theta)))]])))))
(def wasd-fly
  {:press {87 (fn [o] (fn [] (swap! (o :velocity) (partial map + @(o :orientation)))))
           65 (fn [o] (fn [] (swap! (o :velocity) (partial map + (map * [1 0 -1] (reverse @(o :orientation)))))))
           83 (fn [o] (fn [] (swap! (o :velocity) (partial map + (map #(* -1 %) @(o :orientation))))))
           68 (fn [o] (fn [] (swap! (o :velocity) (partial map + (map * [-1 0 1] (reverse @(o :orientation)))))))}
   :release {87 (fn [o] (fn [] (reset! (o :velocity) [0 0 0])))
             65 (fn [o] (fn [] (reset! (o :velocity) [0 0 0])))
             83 (fn [o] (fn [] (reset! (o :velocity) [0 0 0])))
             68 (fn [o] (fn [] (reset! (o :velocity) [0 0 0])))}})
(def arrows-pitch-yaw
  {:press {38 (fn [o] (fn [] (swap! (o :angular-velocity) (partial map + (map * [-3 0 3] (reverse @(o :orientation)))))))
           37 (fn [o] (fn [] (swap! (o :angular-velocity) (partial map + [0 1 0]))))
           40 (fn [o] (fn [] (swap! (o :angular-velocity) (partial map + (map * [3 0 -3] (reverse @(o :orientation)))))))
           39 (fn [o] (fn [] (swap! (o :angular-velocity) (partial map + [0 -1 0]))))}
   :release {38 (fn [o] (fn [] (reset! (o :angular-velocity) [0 0 0])))
           37 (fn [o] (fn [] (reset! (o :angular-velocity) [0 0 0])))
           40 (fn [o] (fn [] (reset! (o :angular-velocity) [0 0 0])))
           39 (fn [o] (fn [] (reset! (o :angular-velocity) [0 0 0])))}})
