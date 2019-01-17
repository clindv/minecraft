(ns minecraft.data
  (:require (minecraft time
                       control))
  (:gen-class))
(defn physics []
  {:position (atom [3 5 2])
   :velocity (atom [0 0 0])
   :acceleration (atom [0 0 0])
   :towards (atom [-0.55 -0.82 0.13])})
(def camera
  (conj (physics)
        [:id (atom "camera")]
        [:tick (atom (minecraft.time/tick :now))]))
(defn refresh [o]
  (let [past-tick @(o :tick)
        delta-tick (- (reset! (o :tick) (minecraft.time/tick :now)) past-tick)]
    (swap! (o :velocity) (partial map + (map (partial * (/ delta-tick 1e3)) @(o :acceleration))))
    (swap! (o :position) (partial map + (map (partial * (/ delta-tick 1e3)) @(o :velocity))))))
(def wasd-fly
  {:press {87 (fn [o] (fn [] (swap! (o :velocity) (partial map + @(o :towards)))))
           65 (fn [o] (fn [] (swap! (o :velocity) (partial map + (map * [1 0 -1] (reverse @(o :towards)))))))
           83 (fn [o] (fn [] (swap! (o :velocity) (partial map + (map #(* -1 %) @(o :towards))))))
           68 (fn [o] (fn [] (swap! (o :velocity) (partial map + (map * [-1 0 1] (reverse @(o :towards)))))))}
   :release {87 (fn [o] (fn [] (swap! (o :velocity) (partial map + (map #(* -1 %) @(o :towards))))))
             65 (fn [o] (fn [] (swap! (o :velocity) (partial map + (map * [-1 0 1] (reverse @(o :towards)))))))
             83 (fn [o] (fn [] (swap! (o :velocity) (partial map + @(o :towards)))))
             68 (fn [o] (fn [] (swap! (o :velocity) (partial map + (map * [1 0 -1] (reverse @(o :towards)))))))}})
