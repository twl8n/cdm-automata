(ns experiment
  (:require [clojure.string :as string]))

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
