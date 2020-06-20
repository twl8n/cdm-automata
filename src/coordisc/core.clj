(ns coordisc.core
  (:require [clojure.string :as string]
            [coordisc.forecast :as fore]
            [clojure.pprint :as pp]))

(defn good-price
  "Choose a price that is randomly within accuracy % of ideal clearing"
  [orig-forecast clearing-price]
  (let [accuracy 0.02 ;; 2 %
        coef (- 1 (rand 0.02))
        bid (int (Math/ceil (* coef clearing-price)))]
    {:id (:id orig-forecast) :bid bid}))

(defn bad-price
  "Choose a price that is how-bad below the ideal clearing price."
  [orig-forecast clearing-price]
  (let [how-bad 0.1
        coef (- 1 how-bad)
        bid (int (Math/ceil (* coef clearing-price)))]
    {:id (:id orig-forecast) :bid bid}))

(defn make-bid [orig-forecast clearing-price]
  (let [bid (int (Math/ceil (* (:coef orig-forecast) clearing-price)))]
    {:id (:id orig-forecast) :bid bid}))

;; How forecast bots behave.
(def forecast [{:id 1
                :fn good-price
                :balance 1000}
               {:id 2
                :fn bad-price
                :balance 1000}])

;; saved price history The initial value is special. Market first forecast is a singularity, and needs more clarity.
(def saved-history [{:id -1
                     :sequence -1
                     :start 1000 ;; current price this period based on previous forecast
                     :end   1001   ;; ditto
                     :pool-balance 0}
                    {:id 0
                     :sequence 0
                     :start 1001 ;; current price this period based on previous forecast
                     :end   1000 ;; ditto
                     :pool-balance 0}])

;; The bots' concept of ideal price. Each bot deviates from this.
;; Really, each bot needs its own view of future ideal prices.

;; Price in cents, in the range of $10K
(def ideal-clearing-price
  (map #(* 10000 %)
       [100 105 105 106 107 108 108 100 99 90 95 95 98 100 100 101 99 102 98 102 103])) 

;; runner returns {:forecast <val> :saved-history <val>}
;; :forecast the forecast hashmap for each bot
;; :saved-history is the updated full history

(defn runner [cp local-saved-history forecast]
  (let [end-price (:end (last local-saved-history))
        forecast-bid-vector (map (fn [mforecast]
                                   (assoc mforecast :bid (:bid ((:fn mforecast) mforecast end-price))))
                                 forecast)]
    (println "End price: " end-price forecast-bid-vector)
    (reduce   
     fore/single-bid-reducer
     {:saved-history local-saved-history :forecast []}
     forecast-bid-vector)))

(defn -main []
  (let [new-history (runner (nth ideal-clearing-price 0) saved-history forecast)]
    (pp/pprint (:saved-history (runner (nth ideal-clearing-price 1) (:saved-history new-history) forecast)))))
  

(comment
  (pp/pprint (runner (nth ideal-clearing-price 0) saved-history forecast))

  ;; Binary search over the range from (current price) to (my bid price)
  ;; Current price is the most recent :end price, which changes in the local history as bids are resolved.
  ;; We start with (:end (last saved-history)), 1000
  ;; The first bidder wants 0.8 which is 800. We need to binary search from 1000 back to 800.
  ;; bid-range: (1280000 1180000 1080000 980000 880000)
  ;; Done: change the number to have fewer zeros
  ;; Todo: use a multipler to test efficiency of the algo.
  (make-bid (first forecast) (:end (last saved-history))) ;; {:id 1, :bid 800}
  (first forecast) ;; {:id 1, :coef 0.8, :balance 1000}
  (merge {:id 1, :coef 0.8, :balance 1000} {:id 1, :bid 800})
  (map (fn [mforecast] (assoc mforecast :bid (:bid (make-bid mforecast (:end (last saved-history)))))) forecast)

  )
