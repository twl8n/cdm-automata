(ns forecast-tests
  (:require [coordisc.forecast :refer :all]))

(defn check-info-metric []
  (and 
   (= (info-metric 995 1005) 0.010000083334582958)
   (= (info-metric 995 1010) 0.01496287267671232)
   (= (info-metric 1010 995 ) 0.014962872676712377)
   (= (info-metric 0 0 ) 0)
   (= (info-metric -1 0 ) 0)
   (= (info-metric -1000 990))))


(def projected-history
  {:id 1
   :sequence 0
   :start 1001 ;; current price this period based on previous forecast
   :end   800 ;; ditto
   :pool-balance 0})

(defn check-total-info []
  (and 
   (= (total-info projected-history 799) 0.22414305164729326)
   (= (total-info projected-history 800) 0.22414305164729326)
   (= (total-info projected-history 810) 0.2117205316487361)
   (= (total-info projected-history 0) 0.22414305164729326)
   (= (total-info projected-history -1) 0.22414305164729326)
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

