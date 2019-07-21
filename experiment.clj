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

(defn foo [xx] (inc xx))

(cond-> 1
  true ((fn foo [xx] (inc xx))))

(def clearing-price [100 105 105 106 107 108 108 100 99 90 95 95 98 100 100 101 99 102 98 102 103]) 

;; amount that buyers and sellers bring to the market

(def fudge-factor -1) ;; -0.1
;; (int (Math/ceil (+ trading-volume (* fudge-factor (exp (- clearing-price final-price) 2)))))

;; (int (Math/ceil (/ 3 2)))

(nth clearing-price 0)

(def spec [{:id 1
            :coef 0.90
            :balance 10000}
           {:id 2
            :coef 1.10
            :balance 1000}])

(defn make-bid [one-spec cp]
  (prn one-spec)
  (let [bid (int (Math/ceil (* (:coef one-spec) cp)))]
    {:id (:id one-spec) :bid bid}))

(defn run-spec [all-spec]
  (map 
   (fn [cp]
     (map(fn [xx] (make-bid xx cp)) all-spec))
     clearing-price))

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
        systematic-return 1.1
        rate-of-return (exp 1.1 period)
        cost-of-bid (int (Math/ceil
                          (/ (+ (apply + (map :pool-balance history)) (* trading-volume commission-share info-share)) (- rate-of-return 1))))] 
    [bid-arg cost-of-bid])
  )

(apply + (map :pool-balance saved-history))

;; ({:id 1, :bid 90} {:id 2, :bid 111})
(def foo (run-spec spec))
(def max-bid (apply max (map :bid (first foo))))
(def min-bid (apply min (map :bid (first foo))))

(def max-bid (apply max (map :bid (second foo))))
(def min-bid (apply min (map :bid (second foo))))

;; Run this on each bid, choose the price, resolve bid, save history, repeat.
(map #(bid-possible-demo % saved-history) (range min-bid (inc max-bid)))



;; Special saved-history initial value

(def saved-history [{:id 0
                     :sequence 0
                     :start 100 ;; current price this period based on previous speculation
                     :end 100   ;; ditto
                     :pool-balance 0}])

(def saved-history [{:id 0
                     :sequence 0
                     :start 100 ;; current price
                     :end 100   ;; also current price
                     :pool-balance 0}
                    {:id 1
                     :sequence 1
                     :start 100
                     :end 111
                     :pool-balance 3}
                    ])

(def saved-history [{:id 0
                     :sequence 0
                     :start 100 ;; current price
                     :end 100   ;; also current price
                     :pool-balance 0}
                    {:id 1
                     :sequence 1
                     :start 100
                     :end 111
                     :pool-balance 3}
                    {:id 2
                     :sequence 2
                     :start 111
                     :end 90
                     :pool-balance 5}])
;; ([90 3] [91 2] [92 2] [93 2] [94 2] [95 2] [96 1] [97 1] [98 1] [99 1] [100 0] [101 1] [102 1] [103 1] [104 1] [105 1] [106 2] [107 2] [108 2] [109 2] [110 2] [111 3])



(def max-bid (apply max (map :bid (first foo))))
(def min-bid (apply min (map :bid (first foo))))

(map #(bid-possible-demo %) (range min-bid (inc max-bid)))


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

(shuffle (first foo))
;; [{:id 2, :bid 111} {:id 1, :bid 90}]

[{:id 1 :bid 90}
  {:id 2 :bid 90}]

;; [{:id 1 :coef 0.9 :balance 1000 :all-future [{ :bid 90} {:bid 95} ...]}
;;  {:id 2 :coef 0.8 :balance 1010 :all-future [{ :bid 90} {:bid 95} ...]}]
 

