(ns coordisc.core
  (:require [clojure.string :as string]
            [coordisc.forecast :as fore]
            [clojure.pprint :as pp]))



(defn -main []
  (let [new-history (fore/runner (nth fore/ideal-clearing-price 0) fore/saved-history fore/forecast)]
    (:saved-history new-history)))

  

