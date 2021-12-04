(ns coordisc.forecast
  (:require [clojure.string :as string]
            [clojure.math.numeric-tower :as math]
            [clojure.pprint :as pp]))

;; This is the library of functions for forecasts. So far, we can resolve price bids. We've got very
;; rudimentary price history, and we've got really rudimentary bot code. Although this code seems to (mostly)
;; work, everything here needs to be validated.

;; A 3 transaction saved history, for demo/comment purposes. Working code examples in comments rely on this
;; def. Whe we have sufficient tests to illustrate calling each fn, you can clean this up.
(def sh3
  [{:id -1, :sequence -1, :start 1000, :end 1001, :pool-balance 0}
   {:id 0, :sequence 0, :start 1001, :end 1000, :pool-balance 0}
   {:id 1,
    :sequence 1,
    :start 1000,
    :actual-cost 976,
    :end 995.0,
    :pool-balance 976}])


(def const-trading-volume 13000)

(defn log [xx]
  (Math/log xx))

(defn abs [xx]
  (Math/abs xx))

(def low-brain
  (let [prefer-low true]
    {:prefer-low prefer-low
     :prefer-high (not prefer-low)
     :id 100}))

(def high-brain
  (let [prefer-low false]
    {:prefer-low prefer-low
     :prefer-high (not prefer-low)
     :id 101}))

(def current-ideal 5.00)
(def cbid 1.00)
(def bids (atom []))
(defn bid [which-brain new-bid]
  (println "bidding with " (:id which-brain))
  (swap! bids (fn [cval] (conj cval {:value new-bid
                                     :id (:id which-brain)}))))


;; amount that buyers and sellers bring to the market

(def fudge-factor -1) ;; -0.1
;; (int (Math/ceil (+ trading-volume (* fudge-factor (exp (- clearing-price final-price) 2)))))

;; (int (Math/ceil (/ 3 2)))

;; https://github.com/clojure/math.numeric-tower

(defn old-exp [x n]
  (reduce * (repeat n x)))

(defn exp [x n]
  (math/expt x n))

(comment
  ;; example precondition
  (defn fx [& args]
    {:pre [(= 2 (count args))]}
    args)

  ;; user=> (fx 1 2)
  ;; (1 2)
  ;; user=> (fx 1 2 3)
  ;; AssertionError Assert failed: (= 2 (count args))  user/fx (NO_SOURCE_FILE:1)
  )

(defn number-check [fn-name xx]
  (when (not (number? xx))
    (throw (Exception. (format "Non-numeric arg `%s` passed to `%s`" xx fn-name)))))

;; Non-numeric args need to throw an error.
(defn between [aa bb cc]
  (doseq [xx [aa bb cc]] (number-check "between" xx))
  (when (not (every? identity (map number? [aa bb cc]))) )
  (or (and (<= aa bb) (<= bb cc))
      (and (>= aa bb) (>= bb cc))
      ))

(comment
  ;; Things that could go wrong in info-metric without sanity checking
  (abs (log -990)) ;; NaN
  (abs (log -0.99)) ;; NaN
  (abs (log 0)) ;; Infinity
  )

(defn info-metric
  "Return zero when the computation is not possible, even if the result is Infinity or NaN"
  [start end]
  (if (= 0 start) 0
      (let [delta-es (/ end start)]
        (if (>= 0 delta-es) 0
            (abs (log delta-es))))))

(defn total-info
  "Total info for this bid and one history transaction. bid-info-share accumulates all historical total-info values."
  [sh bid]
  (let [start (:start sh)
        end (:end sh)
        ti (cond
             ;; (between low mid high)
             (between end start bid) 0
             (between start end bid) (info-metric start end)
             (between start bid end) (info-metric start bid)
             )]
    ti))

(defn test-reduce [local-history intended-bid]
  (let [end (:end (last local-history))]
    (reduce + (info-metric intended-bid end) (map #(total-info % intended-bid) local-history))))

;; (info-metric 800 (:end (last saved-history)))
(comment
  (let [saved-history sh3
        intended-bid 985]
    [(bid-info-share intended-bid saved-history {:id 1
                                                 :sequence 1
                                                 :start (:end (last saved-history))  ;; current price this period based on previous forecast
                                                 :end   intended-bid ;; ditto
                                                 :pool-balance 1})
     (bid-possible-demo intended-bid saved-history {:id 1
                                                    :sequence 1
                                                    :start (:end (last saved-history))  ;; current price this period based on previous forecast
                                                    :end   intended-bid ;; ditto
                                                    :pool-balance 1})])
  )


;; :share is the value you want
;; :reduced is a sanity check/debug value of the reduced info sequence
;; :bid-info is the info-metric of the intended bid
;; :info-seq if the complete info sequence
(defn bid-info-share
  "Accumulate all the history info by reducing with +, then divide current bid's info metric by the accumulated info history."
  [intended-bid local-history current-history]
  (let [end (:end (last local-history))
        bid-info (info-metric intended-bid end)
        info-seq (map #(total-info % intended-bid) local-history)
        reduced-info (reduce + bid-info info-seq)]
    ;; (println "info-seq: " info-seq "bid-info: " bid-info )
    (if (= intended-bid end) 
      {:share 0 :reduced 0 :bid-info bid-info :info-seq info-seq}
      {:share (/ bid-info reduced-info) :reduced reduced-info :bid-info bid-info :info-seq info-seq})))


(defn bid-possible-demo
  "Cost of bid."
  [bid-arg history current-history]
  (let [period (inc (:sequence (last history))) ;; Stay with one-based.
        trading-volume const-trading-volume ;; Previous period trading volume, or some invented starting value
        commission-share 0.009 ;; (aka 0.9 percent)
        {info-share :share} (bid-info-share bid-arg history current-history)
        ;; _ (do (printf "info-share: %s bid-arg: %s\n" info-share bid-arg) (flush))
        systematic-return 1.1
        rate-of-return (exp 1.1 period) 
        parimutuel-pool (apply + (map :pool-balance history))
        cost-of-bid (int (Math/ceil
                          (/ (* (+ parimutuel-pool (* trading-volume commission-share)) info-share) (- rate-of-return 1))))] 
    [bid-arg cost-of-bid]))
(comment
  (* const-trading-volume 0.009 0.0) ;; 0.0
  (apply + (map :pool-balance sh3)) ;; 976
  (exp 1.1 (inc (:sequence (last sh3)))) ;; 1.2100000000000002
  (/ 976.0 (- 1.2100000000000002 1)) ;; 4647.619047619043
  (let [history sh3
        bid-arg 995
        current-history {:id 1
                         :sequence 1
                         :start (:end (last saved-history))  ;; current price this period based on previous forecast
                         :end   bid-arg ;; ditto
                         :pool-balance 1}]
    (bid-info-share bid-arg history current-history));; 0.0

  (let [saved-history sh3
        intended-bid 995]
    (bid-possible-demo intended-bid saved-history {:id 1
                                                   :sequence 1
                                                   :start (:end (last saved-history))  ;; current price this period based on previous forecast
                                                   :end   intended-bid ;; ditto
                                                   :pool-balance 1}))
  )

(defn calc-trading-volume [final-clearing ideal-clearing ideal-volume]
  (let [magic-number 10 
        temp (- final-clearing ideal-clearing)]
    (- ideal-volume (* magic-number (* temp temp)))))


;; Example of things that bots want to know in order to bid.
(def single-forecast {:bid "this forecaster bid, int"
                      :id "id from forecast, int"
                      :coef "coef from forecast, float"
                      :balance "balance from forecast, int"})


(defn found-example-binary-search
  [coll ^long coll-size  target]
  (let [cnt (dec coll-size)]
    (loop [low-idx 0 high-idx cnt]
      (if (> low-idx high-idx)
        nil
        (let [mid-idx (quot (+ low-idx high-idx) 2) mid-val (coll mid-idx)]
          (cond
            (= mid-val target) mid-idx
            (< mid-val target) (recur (inc mid-idx) high-idx)
            (> mid-val target) (recur low-idx (dec mid-idx))))))))
;; (binary-search [1 2 4 5 6 7 8] 8 5)

(defn binary-search
  [min max target]
  )

(defn round-up [nn] (if (pos? nn)
                      (Math/floor (+ nn 0.5))
                      (Math/ceil (+ nn -0.5))))

(defn half-way [orig dest]
  (+ orig (round-up (/ (- dest orig) 2))))

(defn check-bid
  [saved-history intended-bid balance]
  (let [end-price (:end (last saved-history))
        current-history {:id 1
                         :sequence 0
                         :start end-price    ;; current price this period based on previous forecast
                         :end   intended-bid ;; ditto
                         :pool-balance 0}]
    (second (bid-possible-demo intended-bid saved-history current-history))))


(defn find-bid-cost
  "Binary search. 

   When cost exceeds balance, try a new bid closer to the dest.

   When cost is lower than balance, try a new bid further from the dest, and make the dest the previous try-bid.

   When cost exceeds balance and the new-try equals try-bid then we have overshot. Return the previous bid."

  [try-bid dest-bid prev-bid balance saved-history iter]
  (let [try-cost (check-bid saved-history try-bid balance)]
    (println "======== trying:" try-bid "dest:" dest-bid "try-cost:" try-cost)
    (if (or (> 1 try-bid) (> iter 25))
      {:bid try-bid :cost try-cost :succeed false}
      ;; try-cost < balance return [try-cost try-bid success]
      (if (< try-cost balance)  
        {:bid try-bid :cost try-cost :succeed true}
        (let [dest-cost (check-bid saved-history dest-bid balance)
              _ (println "try-cost: " try-cost " dest-cost: " dest-cost)
              [new-try new-dest new-orig]
              (cond
                ;; try-cost > balance and dest-cost < balance recurse half 
                (and (> try-cost balance) (< dest-cost balance)) [(half-way try-bid dest-bid) dest-bid prev-bid]
                ;; try-cost < balance and dest-cost > balance recurse half
                (and (< try-cost balance) (> dest-cost balance)) [(half-way try-bid dest-bid) prev-bid prev-bid]
                :else [try-bid dest-bid prev-bid])
              ;; try-cost > balance and dest-cost > balance return [try-cost try-bid fail]
              bid-cost (if (= new-try try-bid)
                         (if (> try-cost balance)
                           {:bid prev-bid :cost try-cost :succeed false}
                           {:bid try-bid :cost try-cost :succeed true})
                         (find-bid-cost new-try new-dest new-orig balance saved-history (inc iter)))]
          bid-cost)))))

(comment
  (find-bid-cost 980.0 (:end (last saved-history)) 980.0 949.0 saved-history 0)
  (find-bid-cost 980.0 (:end (last saved-history)) 980.0 977.0 saved-history 0)
  (find-bid-cost 1020.0 (:end (last saved-history)) 1020.0 949.0 saved-history 0)
  (find-bid-cost 1020.0 (:end (last saved-history)) 1020.0 977.0 saved-history 0)

  (find-bid-cost 1020.0 (:end (last sh3)) 1020.0 4000.0 sh3 0) ;; {:bid 998.0, :cost 2607, :succeed true}
  (find-bid-cost 1020.0 (:end (last sh3)) 1020.0 5000.0 sh3 0) ;; {:bid 1007.0, :cost 4805, :succeed true}
  (find-bid-cost 1020.0 (:end (last sh3)) 1020.0 8000.0 sh3 0) ;; {:bid 1020.0, :cost 5004, :succeed true}
  (find-bid-cost 995.0 (:end (last sh3)) 1020.0 5000.0 sh3 0) ;; BAD! 995 to 995 cost 0.0 did not succeed. {:bid 995.0, :cost 0, :succeed true}
  (find-bid-cost 996.0 (:end (last sh3)) 996.0 977.0 sh3 0)

  ;; Bug! Bid moved beyond the destination. And the cost was going up on trials, not down. {:bid 985.0, :cost 4997, :succeed true}
  (find-bid-cost 992.0 (:end (last sh3)) 992.0 5000 sh3 0)

  (let [try-bid 992
        dest-bid 995]
    (+ try-bid (round-up (/ (- dest-bid try-bid) 2))))

  (half-way 993 1020) ;; [993 1007.0 1020] if 993 too high recurse [1007.0 1020 993] half dest orig
  (half-way 1007 993) ;; [1007 1000.0 993] if 1007 too low recurse [1000.0 1007 1007] half orig orig
  (half-way 1007 1020) ;; [1007 1014.0 1020] if 1007 too low recurse 
  (half-way 1020 1007) ;; [1020 1013.0 1007]
  )


(defn single-bid-reducer
  "Deprecated. May not work at all. See new code below that does a binary search for an affordable price change"
  [period-record my-forecast]
  (printf "using: %s\n" my-forecast)
  (flush)
  (let [saved-history (:saved-history period-record)
        _ (do (printf "local sh: %s\n" saved-history) (flush))
        cp (:end (last saved-history))
        _ (do (printf "cp: %s\n" cp) (flush))
        bid-wanted (:bid my-forecast) 
        _ (do (printf "bid-wanted: %s for bid %s on %s\n" bid-wanted (:bid my-forecast) cp) (flush))
        bid-range  (if (< cp bid-wanted)
                     (range bid-wanted cp -100000)
                     (range bid-wanted cp 100000))
        _ (do (printf "bid-range: %s\n" bid-range) (flush))
        ;; Run this on each bid, choose the price, resolve bid, save history, repeat.
        ;; bid-cost is [[bid cost] ...]
        ;; need to create a new, ephemeral element of saved-history only for this cost test.
        current-history (conj saved-history {:id 1
                                             :sequence 0
                                             :start 1000001 ;; previous :end
                                             :end   800000 ;; new end based on :bid aka 0.80 or 800000
                                             :pool-balance 0})
        bid-cost (mapv #(bid-possible-demo % saved-history current-history) bid-range)
        _ (def g-bid-cost bid-cost)
        _ (do (printf "bid-wanted: %s\n" bid-wanted) (flush))
        ;; (find-bid-cost 980.0 (:end (last saved-history)) 980.0 949.0 saved-history 0)
        bid-cost (find-bid-cost bid-wanted cp bid-wanted (:balance my-forecast) saved-history 0)]
    ;; resolve bid
    ;; return updated save history, and new forecast
    {:forecast (conj (:forecast period-record)
                     (assoc my-forecast :balance (- (:balance my-forecast) (:cost bid-cost))))
     :saved-history 
     (conj saved-history
           {:id (:id my-forecast)
            :sequence (inc (:sequence (last saved-history)))
            :start (:end (last saved-history))
            :actual-cost (:cost bid-cost)
            :end (:bid bid-cost)
            :pool-balance (+ (:cost bid-cost) (:pool-balance (last saved-history)))})}))


(comment
  (calc-trading-volume 99 100 5000)

  (info-metric 800 1000) ;; 0.22314355131420976
  (info-metric 1000 1000) ;; 0.0
  (total-info {:start 1001 :end 1000} 800)
  (/ 0.5108256237659907 (reduce + 0.51 '(0 9.995003330834993E-4)))
  (let [end-price (:end (last saved-history))
        mforecast {:id 1
                   :coef 0.980
                   :balance 1000} ;; (nth forecast 0)
        intended-bid (merge mforecast (make-bid mforecast end-price))
        current-history {:id 1
                         :sequence 0
                         :start end-price           ;; current price this period based on previous forecast
                         :end   (:bid intended-bid) ;; ditto
                         :pool-balance 0}]
    [(bid-possible-demo (:bid intended-bid) saved-history current-history) intended-bid])

  (check-bid saved-history 999 1000)
  )

(defn udir [origin dest]
  "unit direction, +1 up, -1 down. origin > dest is +1. Origin is current price. Dest is bid price."
  (if (= origin dest)
    0
    (let [delt (- origin dest)]
      (/ (abs delt) delt))))

