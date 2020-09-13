(ns temper.transit-test
  (:require [clojure.test :refer :all]
            [temper.transit :as tt]
            [temper.api :as tm]))

(deftest round-trip
  (let [now (tm/now)]
    (is (= now (-> now tt/->transit tt/<-transit))
        "Round-trip (within platform) works.")))