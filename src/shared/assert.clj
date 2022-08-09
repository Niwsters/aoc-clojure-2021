(ns shared.assert)

(defn assert-equal [expected result]
  (if (not (== expected result))
    (println (str "Expected " result " to equal " expected))
    (print ".")))
