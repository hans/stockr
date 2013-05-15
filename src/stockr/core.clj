(ns stockr.core
  (:require [stockr.dataset :as d]
            [clj-ml.classifiers :as c]))

(defn build-classifier
  [symbol & classifier-args]

  (let [ds (d/build-classification-dataset symbol)
        classifier-args (if (empty? classifier-args)
                          '(:decision-tree :c45)
                          classifier-args)
        classifier (apply c/make-classifier classifier-args)]
    (c/classifier-train classifier ds)))
