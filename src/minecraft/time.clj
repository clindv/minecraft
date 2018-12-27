(ns minecraft.time
  (:gen-class))
(def ^{:private true} clock
  {:start (atom 0)
   :stop (atom 0)
   :paused (atom 0)})
(defmulti tick identity)
(defmethod tick :run [x]
  (reset! (clock :stop) 0)
  (reset! (clock :paused) 0)
  (reset! (clock :start) (System/currentTimeMillis)))
(defmethod tick :now [x]
  (if (zero? @(clock :stop))
    (- (System/currentTimeMillis) @(clock :start) @(clock :paused))
    (- @(clock :stop) @(clock :start) @(clock :paused))))
(defmethod tick :real [x]
  (- (System/currentTimeMillis) @(clock :start)))
(defmethod tick :pause [x]
  (if (zero? @(clock :stop))
    (reset! (clock :stop) (System/currentTimeMillis))
    false))
(defmethod tick :resume [x]
  (if (zero? @(clock :stop))
    false
    (do (swap! (clock :paused) + (- (System/currentTimeMillis) @(clock :stop)))
        (reset! (clock :stop) 0))))
