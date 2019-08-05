(ns experiment
  (:require [clojure.string :as string]))

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


(defn think [which-brain]
  (println "thinking with " (:id which-brain))
  (when (:prefer-low which-brain) (bid which-brain (* current-ideal 0.95)))
  (when (:prefer-high which-brain) (bid which-brain (* current-ideal 1.05))))

(do
  (map think [low-brain high-brain]))

;; Seems like we can write a finite automata inline. Not with cond-> as below, but maybe if cont-> that checked the jump value
;; We need a goto-like behavior.
(def state (atom :start))
(swap! state (fn [xx] :start))

(cond-> (swap! state (fn [xx] :start))
  (= @state :start) ((fn [xx] (printf "starting %s\n" xx) (swap! state (fn [xx] :wait)) :wait))
  (= @state :debug) ((do (println "debugging") :wait))
  (= @state :wait) ((fn [xx] (printf "waiting %s\n" xx ))))

;; amount that buyers and sellers bring to the market

(def fudge-factor -1) ;; -0.1
;; (int (Math/ceil (+ trading-volume (* fudge-factor (exp (- clearing-price final-price) 2)))))

;; (int (Math/ceil (/ 3 2)))

(defn make-bid [one-spec cp]
  (let [bid (int (Math/ceil (* (:coef one-spec) cp)))]
    {:id (:id one-spec) :bid bid}))

(defn run-spec-v1 [all-spec clearing-price]
  (map 
   (fn [cp]
     (map(fn [xx] (make-bid xx cp)) all-spec))
   clearing-price))

(defn run-spec [spec clearing-price]
  "v2"
  (map (fn [cp] (make-bid spec cp)) clearing-price))

;; https://github.com/clojure/math.numeric-tower

(defn exp [x n]
  (reduce * (repeat n x)))

(defn between [aa bb cc]
  (or (and (<= aa bb) (<= bb cc))
      (and (>= aa bb) (>= bb cc))
      ))
  
(defn info-metric [start end]
  (abs (log (/ end start))))

(defn total-info [sh bid]
  (let [start (:start sh)
        end (:end sh)]
  (cond
    (between end start  bid) 0
    (between start end  bid) (info-metric start end)
    (between start bid end) (info-metric start bid)
    )))

;; (total-info {:start 100 :end 111} 107)

(defn test-reduce [local-history intended-bid]
  (let [end (:end (last local-history))]
    (reduce + (info-metric intended-bid end) (map #(total-info % intended-bid) local-history))))

(defn bid-info-share [local-history intended-bid ]
  (let [end (:end (last local-history))
        bid-info (info-metric intended-bid end)]
    (if (= intended-bid end)
      0
      (/ bid-info (reduce + bid-info (map #(total-info % intended-bid) local-history))))))

(defn bid-possible-demo [bid-arg history]
  (let [period (inc (:sequence (last history))) ;; Stay with one-based.
        trading-volume 222 ;; Previous period trading volume, or some invented starting value
        commission-share 0.009 ;; (aka 0.9 percent)
        info-share (bid-info-share history bid-arg)
        _ (printf "info-share: %s bid-arg: %s\n" info-share bid-arg)
        systematic-return 1.1
        rate-of-return (exp 1.1 period)
        cost-of-bid (int (Math/ceil
                          (/ (+ (apply + (map :pool-balance history)) (* trading-volume commission-share info-share)) (- rate-of-return 1))))] 
    [bid-arg cost-of-bid]))

;; The bots' concept of ideal price. Each bot deviates from this.
;; Really, each bot needs its own view of future ideal prices.
(def clearing-price [100 105 105 106 107 108 108 100 99 90 95 95 98 100 100 101 99 102 98 102 103]) 

;; How spec bots behave.
(def spec [{:id 1
            :coef 0.80
            :balance 10000}
           {:id 2
            :coef 1.28
            :balance 10000}])

(defn runner [cp saved-history spec]
  ;; (apply + (map :pool-balance saved-history))
  
  ;; ({:id 1, :bid 90} {:id 2, :bid 111})
  (reduce   
   (fn [period-record my-spec]
     (prn "using: " my-spec)
     (let [saved-history (:saved-history period-record)
           wanted (make-bid my-spec cp)
           bid-wanted (:bid wanted)
           bid-range  (if (< cp bid-wanted)
                        (range bid-wanted cp -1)
                        (range bid-wanted cp 1))
           ;; Run this on each bid, choose the price, resolve bid, save history, repeat.
           bid-cost (map #(bid-possible-demo % saved-history) bid-range)
           _ (printf "bid-wanted: %s bid-cost %s\n" bid-wanted (with-out-str (prn bid-cost)))
           ;; choose price
           [actual-bid actual-cost] (loop [[bid cost] (first bid-cost)]
                                      (if (> (:balance my-spec) cost)
                                        [bid cost]
                                        (recur (rest bid-cost))))]
       ;; resolve bid
       ;; return updated save history, and new spec
       {:spec (conj (:spec period-record)
                    (assoc my-spec :balance (- (:balance my-spec) actual-cost)))
        :saved-history 
        (conj saved-history
              {:id (:id my-spec)
               :sequence (inc (:sequence (last saved-history)))
               :start (:end (last saved-history))
               :actual-cost actual-cost
               :end actual-bid
               :pool-balance (+ actual-cost (:pool-balance (last saved-history)))})}))
   {:saved-history saved-history :spec []}
   spec))

  
(comment
  (require '[clojure.pprint :as pp])
  (pp/pprint (runner (nth clearing-price 0) saved-history spec))

  ;; reduce complex value
  (reduce
   (fn [yy xx] (prn xx (:spec yy))
     {:spec (conj (:spec yy) {:id xx}) :balance (+ xx  (:balance yy))})
   {:spec [] :balance 10}
   [1 2 1 4])
  )
  
;; Special saved-history initial value

(def saved-history [{:id 0
                     :sequence 0
                     :start 100 ;; current price this period based on previous speculation
                     :end 100   ;; ditto
                     :pool-balance 0}])

(def ex-saved-history-2 [{:id 0
                          :sequence 0
                          :start 100 ;; current price
                          :end 100   ;; also current price
                          :pool-balance 0}
                         {:id 1
                          :sequence 1
                          :start 100
                          :end 90
                          :pool-balance 3}
                         ])
;; (runner ex-saved-history-2)

(def ex-saved-history-3 [{:id 0
                          :sequence 0
                          :start 100 ;; current price
                          :end 100   ;; also current price
                          :pool-balance 0}
                         {:id 1
                          :sequence 1
                          :start 100
                          :end 90
                          :pool-balance 3}
                         {:id 2
                          :sequence 2
                          :start 90
                          :end 111
                          :pool-balance 5}])

;; ([90 3] [91 2] [92 2] [93 2] [94 2] [95 2] [96 1] [97 1] [98 1] [99 1] [100 0] [101 1] [102 1] [103 1] [104 1] [105 1] [106 2] [107 2] [108 2] [109 2] [110 2] [111 3])

;; (defn resolve-bids [bid spec history]
;;   ([] [{:price 98 :cost-of-bid 0 :id nil}])
;;   ([history] 
;;    ;; save history for next period
;;    ;; use history for pay-out to speculators
;;    )
;;   ([history bid]
;;     ;; accumulate history
;;     (let [cost-of-bid (bid-possible spec)]
;;           ;; loop until cost-of-bid is below :balance
;;           (if (bid-possible)
;;             (conj history {:price 111 :cost-of-bid cost-of-bid :id (:id spec)))
;;             (search-possible))
;;     )))

;; (defn wrap-bids []
;;   (transduce
;;    (run-spec %)
;;    resolve-bids)
;;   spec)

;; [{:id 2, :bid 111} {:id 1, :bid 90}]

[{:id 1 :bid 90}
  {:id 2 :bid 90}]

;; [{:id 1 :coef 0.9 :balance 1000 :all-future [{ :bid 90} {:bid 95} ...]}
;;  {:id 2 :coef 0.8 :balance 1010 :all-future [{ :bid 90} {:bid 95} ...]}]
 

