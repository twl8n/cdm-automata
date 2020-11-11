(ns forecast-tests
  (:require [coordisc.forecast :refer :all]))

(comment
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
;; There could be an issue with multiple entries in a history this is computing information content of a second bid
;; if the intent is to sum up the information content of all bids then the end bid of the final bid in the history is end the start of each
;; entry in the history is start and the end of each element of the history is bid and then that list trasform would be summed.
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


(defn -main [& args]
  (let [test-results
        [(check-info-metric)
         (check-total-info)]]
  (printf "%s tests performed: %s passed: %s\n" 
          (ns-name *ns*)
          (count test-results)
          (count (filter true? test-results)))
    (flush)))

