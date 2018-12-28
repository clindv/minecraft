(ns minecraft.core
  (:require (minecraft control
                       time))
  (:gen-class))
(set! *warn-on-reflection* true)
(minecraft.time/tick :run)
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
