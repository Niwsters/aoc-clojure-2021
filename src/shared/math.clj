(ns shared.math)

(defn transpose [matrix]
  (apply mapv vector matrix))
