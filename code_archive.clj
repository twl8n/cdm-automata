(defn run-spec-v1 [all-spec clearing-price]
  (map 
   (fn [cp]
     (map(fn [xx] (make-bid xx cp)) all-spec))
   clearing-price))

(defn run-spec [spec clearing-price]
  "v2"
  (map (fn [cp] (make-bid spec cp)) clearing-price))


  (defn old-traverse
  "Must have a starting state aka edge. jump-stack initially is empty. Return a map with keys wait-next, msg."
  [curr-state jump-stack]
  (prn "traverse curr-state: " curr-state " js: " jump-stack)
  (loop [st (sub-table curr-state table)]
    (cond (empty? st)
          (do
            (prn "table is empty for edge: " curr-state)
            false)
          (nil? (read-line)) ;; ^D will cause nil input
          (do
            (prn "read-line returned nil")
            false)
          :else
          (let [smap (first st) ;; state map, state table
                ;; this old test for the :edge to match curr-state is wrong because we use sub-table
                ;; (and (= (smap :edge) curr-state)
                fres (cond (if ((smap :test))
                             (do
                               (if (dispatch (smap :func))
                                 (traverse (smap :next) jump-stack)
                                 false)))
                           ;; if true we really want to do a cond. The if-cond should be an fn.
                           (cond ((smap :func)) ;; never nil? because empty :func becomes fntrue
                                 (traverse (smap :next) jump-stack)
                                 (is-jump? (smap :func))
                                 (apply traverse (jump-to (smap :func) jump-stack))
                                 (is-return? (smap :func))
                                 (traverse (first jump-stack) (rest jump-stack))
                                 (is-wait? (smap :func))
                                 (do
                                   (prn "is-wait? true")
                                   ;; (smap :next)
                                   false))
                           :else
                           false)]
            (prn "fres: " fres " post-let js: " jump-stack " edge: " (smap :edge))
            (if fres
              (do
                (prn "fres is true")
                false)
              (recur (rest st))))
          )) false)

