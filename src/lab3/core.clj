(ns lab3.core
  (:import (java.io BufferedReader))
  (:use clojure.test))
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(def input (csv/read-csv (BufferedReader. *in*) :separator \;))
(defn to-float [x] (Float/parseFloat x))

(defn approximate [x1 y1 x2 y2 step]
  (map #(+ (/ (- (* x2 y1) (* x1 y2)) (- x2 x1)) (* % (/ (- y2 y1) (- x2 x1)))) (range x1 x2 step))
  )

(defn approximate-reader
  [step]
  (loop [flow input
         previous nil]
    (if (nil? previous)
      ()
      (apply println (map #(str "\t" (format "%.3f" %1) ";" (format "%.3f" %2) "\n") (range (to-float (first previous)) (to-float (first (first flow))) step)
                          (approximate (to-float (first previous)) (to-float (second previous)) (to-float (first (first flow))) (to-float (second (first flow))) step))))
    (recur (next flow) (first flow))
    )
  )


(defn factors [x x-ij x-values]
  (map #(/ (- x %) (- x-ij %)) (filter #(not= % x-ij) x-values))
  )

(defn basic-polynomial [x x-values]
  (map #(reduce * 1 (factors x % x-values)) x-values)
  )

(defn lagrange [x-first x-last
                x-values y-values
                step]
  (map #(reduce + (map * (basic-polynomial % x-values) y-values)) (range x-first x-last step))
  )

(defn lagrange-reader
  [step quantity]
  (loop [curr-point (- quantity 1)
         prev-point 0]
    (apply println (map #(str "\t" (format "%.3f" %1) ";" (format "%.3f" %2) "\n") (range (to-float (first (nth input prev-point))) (to-float (first (nth input curr-point))) step)
                        (lagrange (to-float (first (nth input prev-point)))
                                  (to-float (first (nth input curr-point)))
                                  (map #(to-float (first %)) (take quantity (drop prev-point input)))
                                  (map #(to-float (second %)) (take quantity (drop prev-point input)))
                                  step)))
    (recur (inc curr-point) (inc prev-point))
    )
  )

(defn -main
  [method step & other]
  (if (= method "approximate")
    (approximate-reader (to-float step))
    (lagrange-reader (to-float step) (to-float (first other)))
    )
  )
