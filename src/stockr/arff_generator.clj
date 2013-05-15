(ns stockr.arff-generator
  (require [[monger.core :as m]
            [clj-ml.io :as i]]))

(def *moving-average-width*
  "The width of the window used to build a moving average of stock
   quotes."

  5)

(def *change-threshold*
  "The minimum change in stock price required such that the change be
   considered a 'drop' or 'rise' rather than a 'hold.'"

  0.04)

(def attributes-fetch
  "Instance attributes which may be directly fetched from documents"

  '(:LastPrice :Change :Volume :MarketCap))

(def attributes-common
  "Attributes which are common to all models but cannot be directly
  fetched from a document"

  '(:MovingAverage :VolumeChange))

(def attributes-regression
  "Attributes used exclusively in the regression model"

  (concat attributes-fetch attributes-common
          '(:NextHigh :NextLow :NextVolume)))

(def attributes-classification
  "Attributes used exclusively in the classification model"

  (concat attributes-fetch attributes-common
          '(:NextBehavior)))

(defmulti get-attribute
  "Fetch an attribute for a given instance."
  (fn [attr _ _] attr))

(defmethod get-attribute :MovingAverage [attr instances i]
  (let [lower-bound (max 0 (- i *moving-average-width*))
        xs (take *moving-average-width* (drop lower-bound instances))]))

(defmethod get-attribute :VolumeChange [attr instances i]
  (let [instance (nth instances i)
        prev-instance (nth instances (dec i))]
    (- ("Volume" instance) ("Volume" prev-instance))))

(defmethod get-attribute :NextHigh [attr instances i]
  ("High" (nth instances (inc i))))

(defmethod get-attribute :NextLow [attr instances i]
  ("Low" (nth instances (inc i))))

(defmethod get-attribute :NextVolume [attr instances i]
  ("Volume" (nth instances (inc i))))

(defmethod get-attribute :NextBehavior
  "Provide a class label for an example which specifies how the
   referenced stock behaves in the following instance.

   - A `drop` class indicates that the example which directly follows
     this one has a price which has dropped more than
     `*change-threshold` relative to the current price.
   - A `hold` class indicates that the share price for this example and
     the one which directly follows it do not change more than +/-
     `*change-threshold*`.
   - A `rise` class indicates that the example which directly follows
     this one has a price which has risen more than `*change-threshold`
     relative to the current price."
  [attr instances i]

  (let [price ((nth instances i) "LastPrice")
        next-price ((nth instances (inc i)) "LastPrice")
        price-change (- next-price price)]
    (cond
     (> price-change *change-threshold*) :rise
     (< price-change (- *change-threshold*)) :drop
     :else :h)))

;; All other attributes should come from the instance document
(defmethod get-attribute :default [attr instances i]
  (attr (nth instances i)))

(defn make-example
  "Make an example vector with the given attributes."
  [i instances attributes]

  (map #(get-attribute % instances i) attributes))

(defn stock-quotes
  "Get all quotes for a given symbol."
  [symbol]

  )
