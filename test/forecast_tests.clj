(ns forecast-tests
  (:require [coordisc.forecast :refer :all]))

(comment
  ;; With deps.edn, you can do this: clj -A:test -m forecast-tests

  

  
￼  ;; info-metric requires 2 positive inputs because of the symmetry of (abs(log (/ a b))) both orderings of start and end will have the same output.
￼  ;; Its sensible to return 0 for invalid inputs since the system should register that as no information offered and do nothing as a result
  ;; from a UI perspective validating inputs to cleanse zeros and negatives is a good idea.
￼  )

(defn check-info-metric []
  (and 
   (= (info-metric 995 1005) 0.010000083334582958)
   (= (info-metric 995 1010) 0.01496287267671232)
   (= (info-metric 1010 995 ) 0.014962872676712377)
   (= (info-metric 0 0 ) 0)
   (= (info-metric -1 0 ) 0)
   (= (info-metric -1000 990) 0)))


(def projected-history
  {:id 1
   :sequence 0
   :start 1001 ;; current price this period based on previous forecast
   :end   800 ;; ditto
   :pool-balance 0})

(defn check-total-info []
  (and 
   (= (total-info projected-history 799) 0.22414305164729326) 
    ;; All positive numbers less than 800 will have the same value becasue the history indicated a falling price that ended at 800
   (= (total-info projected-history 800) 0.22414305164729326)
   (= (total-info projected-history 810) 0.2117205316487361)
    ;; comming up shourt of 800 gives less than full value correct
   (= (total-info projected-history 0) 0.22414305164729326)  ;; This and the next line are invalid inputs it isn't great if they are here
   (= (total-info projected-history -1) 0.22414305164729326) ;; but for the values above the best outcome for these is 0
   (= (total-info projected-history 999) 0.00200000066666711)
   (= (total-info projected-history 1000) 9.995003330834993E-4)
   (= (total-info projected-history 1001) 0)
   (= (total-info projected-history 2000) 0)
  ))

;; There could be an issue with multiple entries in a history this is computing information content of a second bid
;; if the intent is to sum up the information content of all bids then the end bid of the final bid in the history is end the start of each
;; entry in the history is start and the end of each element of the history is bid and then that list trasform would be summed.

(def projected-history
  {:id 1
   :sequence 0
   :start 1001 ;; current price this period based on previous forecast
   :end   800 ;; ditto
   :pool-balance 0})

(def sh2up
  [{:id -1, :sequence -1, :start 1000, :end 1001, :pool-balance 0}
   {:id 0, :sequence 0, :start 1001, :end 1005, :pool-balance 0}])

(def sh2up-and-back
  [{:id -1, :sequence -1, :start 1000, :end 1001, :pool-balance 0}
   {:id 0, :sequence 0, :start 1001, :end 1000, :pool-balance 0}])

(def sh4
  [{:id -1 :sequence -1 :start 1000 :end 1001 :pool-balance 0}
   {:id 0 :sequence 0 :start 1001 :end 1000 :pool-balance 0}
   {:id 1
    :sequence 1
    :start 1000
    :actual-cost 976
    :end 995.0
    :pool-balance 976}
   {:id 2
    :sequence 2
    :start 995
    :actual-cost 976
    :end 990.0
    :pool-balance 976}])

(defn new-bid-entry
  "Helper fn to simplify scaffolding data"
  [intended-bid hist-seq]
  {:id 1
   :sequence 1
   :start (:end (last hist-seq))  ;; current price this period based on previous forecast
   :end   intended-bid ;; ditto
   :pool-balance 1})

;; Check the debug values returned by bid-info-share.
(defn check-bid-info-share []
  (= (map (fn [[ib mh]] (coordisc.forecast/bid-info-share ib mh (new-bid-entry ib mh))) [[990 sh2up] [1010 sh2up] [990 sh2up-and-back] [980 sh4]] )
     (list {:share 1.0, :reduced 0.015037877364540283, :bid-info 0.015037877364540283, :info-seq (list 0 0)}
           {:share 0.49875621377443163, :reduced 0.009950330853168064, :bid-info 0.004962789342129014, :info-seq (list 9.995003330834232E-4 0.003988041177955627)}
           {:share 0.909546140213648, :reduced 0.011049836186584784, :bid-info 0.010050335853501286, :info-seq (list 0 9.995003330834993E-4)}
           {:share 0.4788355831299119, :reduced 0.021202207650602874, :bid-info 0.010152371464017908, :info-seq (list 0 9.995003330834993E-4 0.005012541823544286 0.005037794029957181)})))

(defn -main [& args]
  (let [test-results
        [(check-info-metric)
         (check-total-info)
         (check-bid-info-share)]]
  (printf "%s tests performed: %s passed: %s\n" 
          (ns-name *ns*)
          (count test-results)
          (count (filter true? test-results)))
    (flush)))

