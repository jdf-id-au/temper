(ns temper.transit-test
  (:require [clojure.test :refer :all]
            [temper.transit :as tt]
            [temper.api :as t]))

(deftest round-trip
  (let [now (t/now)]
    (is (= now (-> now tt/->transit tt/<-transit))
        "Round-trip (same platform) works.")))