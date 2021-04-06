(ns temper.transit-test
  (:require [clojure.test :refer :all]
            [temper.transit :as tt]
            [temper.api :as tm]
            [cognitect.transit :as transit])
  (:import (java.io ByteArrayInputStream ByteArrayOutputStream)))

(defn ->transit "Encode data structure to transit."
  [arg]
  (let [out (ByteArrayOutputStream.)
        writer (transit/writer out :json tt/write-handlers)]
    (transit/write writer arg)
    (.toString out)))

(defn <-transit "Decode data structure from transit."
  [json]
  (let [in (ByteArrayInputStream. (.getBytes json))
        reader (transit/reader in :json tt/read-handlers)]
    (transit/read reader)))

(deftest round-trip
  (let [now (tm/now)]
    (is (= now (-> now ->transit <-transit))
        "Round-trip (within platform) works.")))