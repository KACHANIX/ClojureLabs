(ns playground.pg)
(require '[clojure.core.reducers :as r])

(def a (map inc))

(println (transduce a conj (range 5)))