(ns lab3.core
  (:import (java.io BufferedReader)))
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(def data-flow (csv/read-csv (BufferedReader. *in*) :separator \;))
(defn to-float [x] (Float/parseFloat x))
(defn out [x] (apply println x))

(defn factors
  [x
   x-to-eliminate
   x-values-nodes]
  (map #(/ (- x %) (- x-to-eliminate %)) (filter #(not= % x-to-eliminate) x-values-nodes))
  )

(defn basic-pol-coll
  [x
   x-values-nodes]
  (map #(reduce * 1 (factors x % x-values-nodes)) x-values-nodes)
  )


(defn approximate [x1 y1 x2 y2 step]
  (map #(+ (/ (- (* x2 y1) (* x1 y2)) (- x2 x1)) (* % (/ (- y2 y1) (- x2 x1)))) (range x1 x2 step))
  )

(defn approximate-reader
  [step]
  (loop [flow data-flow
         previous nil]
    (if (nil? previous)
      ()
      (apply println (map #(str "\t" (format "%.3f" %1) ";" (format "%.3f" %2) "\n") (range (to-float (first previous)) (to-float (first (first flow))) step)
                          (approximate (to-float (first previous)) (to-float (second previous)) (to-float (first (first flow))) (to-float (second (first flow))) step)))
      )
    (recur (next flow) (first flow)))
  )

(defn lagrange [x-start x-end
                x-values-nodes y-values-nodes
                step]
  (map #(reduce + (map * (basic-pol-coll % x-values-nodes) y-values-nodes)) (range x-start x-end step))
  )

(defn lagrange-reader
  [step quantity]
  (loop [curr-point (- quantity 1)
         prev-point 0]


    (apply println (map #(str "\t" (format "%.3f" %1) ";" (format "%.3f" %2) "\n") (range (to-float (first (nth data-flow prev-point))) (to-float (first (nth data-flow curr-point))) step)
                        (lagrange (to-float (first (nth data-flow prev-point)))
                                  (to-float (first (nth data-flow curr-point)))
                                  (map #(to-float (first %)) (take quantity (drop prev-point data-flow)))
                                  (map #(to-float (second %)) (take quantity (drop prev-point data-flow)))
                                  step)))
    (recur (inc curr-point) (inc prev-point))
    )
  )


(defn -main
  "docstring"
  [method step & other]
  (println "GOVNO")
  (if (= method "approximate")
    (approximate-reader (to-float step))
    (lagrange-reader (to-float step) (to-float (first other)))
    )
  )
