(ns minecraft.time-test
  (:require [clojure.test :refer :all]
            [minecraft.time :refer :all]))
(deftest tick-test
  (testing "tick"
    (is (tick :run))
    (is (tick :now))
    (let [a (tick :pause)
          b (tick :now)]
      (is a)
      (is (not (tick :pause)))
      (Thread/sleep 1)
      (is (= b (tick :now)))
      (is (not (= b (tick :real))))
      (is (tick :resume))
      (is (not (tick :resume))))))
