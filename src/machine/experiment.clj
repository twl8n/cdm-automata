(ns machine.experiment
  (:require [clojure.string :as string]
            [clojure.pprint :as pp]))


(def const-trading-volume 13000)

;; How spec bots behave.
(def spec [{:id 1
            :coef 0.998
            :balance 1000}
           {:id 2
            :coef 1.02
            :balance 1000}])

;; Special saved-history initial value
(def saved-history [{:id -1
                     :sequence -1
                     :start 1000 ;; current price this period based on previous speculation
                     :end   1001   ;; ditto
                     :pool-balance 0}
                    {:id 0
                     :sequence 0
                     :start 1001 ;; current price this period based on previous speculation
                     :end   1000 ;; ditto
                     :pool-balance 0}])


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

(defn experimental-fsm-start []
  (cond-> (swap! state (fn [xx] :start))
    (= @state :start) ((fn [xx] (printf "starting %s\n" xx) (swap! state (fn [xx] :wait)) :wait))
    (= @state :debug) ((do (println "debugging") :wait))
    (= @state :wait) ((fn [xx] (printf "waiting %s\n" xx )))))

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

(def projected-history
  {:id 1
   :sequence 0
   :start 1001 ;; current price this period based on previous speculation
   :end   800 ;; ditto
   :pool-balance 0})

;; (total-info projected-history 800)
(defn total-info [sh bid]
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
(defn bid-info-share [local-history current-history intended-bid ]
  (let [end (:end (last local-history))
        bid-info (info-metric intended-bid end)
        info-seq (map #(total-info % intended-bid) local-history)]
    ;; (println "info-seq: " info-seq "bid-info: " bid-info )
    (if (= intended-bid end) 
      0
      (/ bid-info (reduce + bid-info info-seq)))))



(defn bid-possible-demo
  [bid-arg history current-history]
  (let [period (inc (:sequence (last history))) ;; Stay with one-based.
        trading-volume const-trading-volume ;; Previous period trading volume, or some invented starting value
        commission-share 0.009 ;; (aka 0.9 percent)
        info-share (bid-info-share history current-history bid-arg)
        ;; info-share (bid-info-share history bid-arg)
        ;; _ (do (printf "info-share: %s bid-arg: %s\n" info-share bid-arg) (flush))
        systematic-return 1.1
        rate-of-return (exp 1.1 period)
        cost-of-bid (int (Math/ceil
                          (/ (+ (apply + (map :pool-balance history)) (* trading-volume commission-share info-share)) (- rate-of-return 1))))] 
    [bid-arg cost-of-bid]))

;; The bots' concept of ideal price. Each bot deviates from this.
;; Really, each bot needs its own view of future ideal prices.

;; Price in cents, in the range of $10K
(def ideal-clearing-price
  (map #(* 10000 %)
       [100 105 105 106 107 108 108 100 99 90 95 95 98 100 100 101 99 102 98 102 103])) 

(defn calc-trading-volume [final-clearing ideal-clearing ideal-volume]
  (let [magic-number 10 
        temp (- final-clearing ideal-clearing)]
        (- ideal-volume (* magic-number (* temp temp)))))


;; Example of things that bots want to know in order to bid.
(def single-spec {:bid "this speculator bid, int"
                  :id "id from sped, int"
                  :coef "coef from spec, float"
                  :balance "balance from spec, int"})


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


(defn single-bid-reducer
  "Deprecated. May not work at all. See new code below that does a binary search for an affordable price change"
  [period-record my-spec]
  (printf "using: %s\n" my-spec)
  (flush)
  (let [saved-history (:saved-history period-record)
        _ (do (printf "local sh: %s\n" saved-history) (flush))
        cp (:end (last saved-history))
        _ (do (printf "cp: %s\n" cp) (flush))
        bid-wanted (:bid my-spec) ;; Some old stuff: (* (:bid my-spec) cp)
        _ (do (printf "bid-wanted: %s for bid %s on %s\n" bid-wanted (:bid my-spec) cp) (flush))
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
        ;; Replace this with a binary search. This works, but for large ranges it is very slow.
        ;; choose price, crawl actual-cost unil reaching a bid I can afford.
        [actual-bid actual-cost] (loop [my-bid-cost bid-cost]
                                   (let [[bid cost] (first my-bid-cost)]
                                     (if (> (:balance my-spec) cost)
                                       [bid cost]
                                       (recur (rest my-bid-cost)))))]
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


(defn runner [cp saved-history spec]
  (let [end-price (:end (last saved-history))
        spec-bid-vector (map (fn [mspec] (assoc mspec :bid (:bid (make-bid mspec end-price)))) spec)]
    (prn "End price: " end-price spec-bid-vector)
    (reduce   
     single-bid-reducer
     {:saved-history saved-history :spec []}
     spec-bid-vector)))
   
  
(comment
  (pp/pprint (runner (nth clearing-price 0) saved-history spec))
  ;; reduce complex value
  (reduce
   (fn [yy xx] (prn xx (:spec yy))
     {:spec (conj (:spec yy) {:id xx}) :balance (+ xx  (:balance yy))})
   {:spec [] :balance 10}
   [1 2 1 4])

  (calc-trading-volume 99 100 5000)


  ;; Binary search over the range from (current price) to (my bid price)
  ;; Current price is the most recent :end price, which changes in the local history as bids are resolved.
  ;; We start with (:end (last saved-history)), 1000
  ;; The first bidder wants 0.8 which is 800. We need to binary search from 1000 back to 800.
  ;; bid-range: (1280000 1180000 1080000 980000 880000)
  ;; Done: change the number to have fewer zeros
  ;; Todo: use a multipler to test efficiency of the algo.
  (make-bid (first spec) (:end (last saved-history))) ;; {:id 1, :bid 800}
  (first spec) ;; {:id 1, :coef 0.8, :balance 1000}
  (merge {:id 1, :coef 0.8, :balance 1000} {:id 1, :bid 800})
  (map (fn [mspec] (assoc mspec :bid (:bid (make-bid mspec (:end (last saved-history)))))) spec)


  ;; @@
  (info-metric 800 1000) ;; 0.22314355131420976
  (info-metric 1000 1000) ;; 0.0
  (total-info {:start 1001 :end 1000} 800)
  (/ 0.5108256237659907 (reduce + 0.51 '(0 9.995003330834993E-4)))
  (let [end-price (:end (last saved-history))
        mspec {:id 1
               :coef 0.980
               :balance 1000} ;; (nth spec 0)
        intended-bid (merge mspec (make-bid mspec end-price))
        current-history {:id 1
                         :sequence 0
                         :start end-price           ;; current price this period based on previous speculation
                         :end   (:bid intended-bid) ;; ditto
                         :pool-balance 0}]
    [(bid-possible-demo (:bid intended-bid) saved-history current-history) intended-bid])

  (check-bid saved-history 999 1000)
  )

(defn check-bid
  [saved-history intended-bid balance]
  (let [end-price (:end (last saved-history))
        current-history {:id 1
                         :sequence 0
                         :start end-price    ;; current price this period based on previous speculation
                         :end   intended-bid ;; ditto
                         :pool-balance 0}]
    (second (bid-possible-demo intended-bid saved-history current-history))))

(defn find-ok-bid
  [saved-history intended-bid balance]
  (when (= 0 intended-bid)
    nil)
  (if (< balance (check-bid saved-history intended-bid balance))
    intended-bid
    (find-ok-bid saved-history (/ intended-bid 2) balance)))

(defn -main []
  (pp/pprint (runner (nth ideal-clearing-price 0) saved-history spec)))

(defn round-up [nn] (Math/floor (- nn 0.5)))
  
;; (find-demo 980.0 (:end (last saved-history)) 949.0 saved-history 0)
;; (find-demo 1020.0 (:end (last saved-history)) 949.0 saved-history 0)
(defn find-demo
  "Only works for lowering price. Needs directionality fix."
  [try-bid max-bid balance saved-history iter]
  (println "======== trying:" try-bid "max:" max-bid)
  (if (or (> 1 try-bid) (> iter 20))
    nil
    (let [cost (check-bid saved-history try-bid balance)]
      (cond (> cost balance) (do
                               (let [new-try (+ (round-up (/ (- max-bid try-bid) 2)) try-bid)]
                               (println ">>> cost balance cost:" cost "try-bid:" try-bid "new-try:" new-try)
                                 (if (= new-try try-bid)
                                   (do (println ">>>> new-try is equal") max-bid)
                                   (find-demo new-try max-bid balance saved-history (inc iter)))))
            (and (< cost balance) (= try-bid max-bid)) (do
                                                         (println "cost:" cost "try-bid:" try-bid)
                                                         try-bid)
            (< cost balance) (if (< try-bid max-bid)
                               (do
                                 (println "<<< cost balance cost:" cost "try-bid:" try-bid)
                                 (let [new-try (- try-bid (round-up (/ (- max-bid try-bid) 2)))]
                                   (if (= new-try try-bid)
                                     (do (println "<<<<< new-try is equal") try-bid)
                                     (find-demo new-try try-bid balance saved-history (inc iter)))))
                               (do (println "<<< try gt max") max-bid))
            ))))
        
