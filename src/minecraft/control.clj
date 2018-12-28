(ns minecraft.control
  (:require (minecraft time))
  (:gen-class))
(defn- init [x]
  (doall (apply hash-map (interleave (range 300) (repeatedly #(atom x))))))
(def keyboard {:tick (init nil)
               :press (init {})
               :release (init {})
               :type (init {})})
(defmulti stroke (fn [_ keycode] _))
(defmethod stroke :press [_ keycode]
  (let [tick ((keyboard :tick) keycode)]
    (if @tick nil (do (doseq [f (vals @((keyboard :press) keycode))] (f))
                      (reset! tick (minecraft.time/tick :now))))))
(defmethod stroke :release [_ keycode]
  (let [tick ((keyboard :tick) keycode)]
    (if @tick (do (doseq [f (vals @((keyboard :release) keycode))] (f))
                  (reset! tick nil)))))
(defmulti leash (fn [obj & more] (nil? (obj :id))))
(defmethod leash false [marionette & controllers]
  (doseq [controller controllers
          [ka va] (map identity controller)
          [kb vb] (map identity va)]
    (swap! ((keyboard ka) kb) conj [(marionette :id) (vb marionette)])))
(defmethod leash true [controller & marionettes]
  (doseq [marionette marionettes
          [ka va] (map identity controller)
          [kb vb] (map identity va)]
    (swap! ((keyboard ka) kb) conj [(marionette :id) (vb marionette)])))
(defmulti unleash (fn [obj & more] (nil? (obj :id))))
(defmethod unleash false [marionette & controllers]
  (if (empty? controllers)
    (doseq [[ka va] (map identity (dissoc keyboard :tick))
            [kb vb] (map identity va)]
      (if (@vb (marionette :id))
        (swap! vb dissoc (marionette :id))))
    (doseq [controller controllers
            [ka va] (map identity controller)
            [kb vb] (map identity va)]
      (if (@((keyboard ka) kb) (marionette :id))
        (swap! ((keyboard ka) kb) dissoc (marionette :id))))))
(defmethod unleash true [controller & marionettes]
  (if (empty? marionettes)
    (doseq [[ka va] (map identity controller)
            [kb vb] (map identity va)]
      (reset! ((keyboard ka) kb) {}))
    (doseq [marionette marionettes
            [ka va] (map identity controller)
            [kb vb] (map identity va)]
      (if (@((keyboard ka) kb) (marionette :id))
        (swap! ((keyboard ka) kb) dissoc (marionette :id))))))
