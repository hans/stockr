(ns stockr.realtime
  (:require [stockr.core :as c]
            [stockr.dataset :as d]
            [clj-ml.classifiers :as ml-c]
            [clj-ml.data :as ml-d])
  (:use [backtype.storm clojure config])
  (:import [backtype.storm LocalCluster]))

(def quote-fields ["LastPrice" "Change" "Volume" "MarketCap" "MovingAverage"
                   "VolumeChange"])

(def quote-field-keywords (map keyword quote-fields))

(let [[a b] (c/build-classifier "GOOG")]
  (def classifier a)
  (def quote-dataset b))

;; DEV
(def quotes
  "Quote data that can be emitted from a spout."
  (let [quotes (d/stock-quotes "GOOG")]
    (map (fn [i]
         (map (comp #(d/get-attribute % quotes i) keyword) quote-fields))
       (range 1 (count quotes)))))

(defspout quote-spout quote-fields
  [conf context collector]
  (spout
   (nextTuple []
              (Thread/sleep 100)
              (emit-spout! collector (rand-nth quotes)))))

(defbolt classify-bolt ["label"] [tuple collector]
  (let [fields (->> tuple vec flatten (apply hash-map))]
    (let [instance (ml-d/make-instance quote-dataset fields)
          _ (ml-c/classifier-classify classifier instance)
          label (ml-c/classifier-classify classifier instance)]

      (emit-bolt! collector [label] :anchor tuple)
      (ack! collector tuple))))

(def stockr-topology
  (topology
   {"1" (spout-spec quote-spout)}
   {"2" (bolt-spec {"1" :shuffle}
                   classify-bolt
                   :p 4)}))

(defn run-local! []
  (let [cluster (LocalCluster.)]
    (.submitTopology cluster "stockr" {TOPOLOGY-DEBUG false} stockr-topology)
    (Thread/sleep 10000)
    (.shutdown cluster)))

(defn -main []
  (run-local!))
