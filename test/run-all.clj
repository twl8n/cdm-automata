

(println "Running tests with Java version" (System/getProperty "java.vm.version"))

;; concatenate a call to -main on the end of the test file string. Using load-string this way obviates a need
;; for in-ns to switch namespaces on the fly. And keeping -main separate prevents the test from running
;; everytime the file is eval'd in cider. We don't want a call to -main at the end of the test file because we
;; don't want the test running everytime we eval the file in cider.

(load-string (str (slurp "test/forecast_tests.clj") "(-main)"))
                  
