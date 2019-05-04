(ns lab3.core
  (:import (java.io BufferedReader)))



(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(defn to-float [x] (Float/parseFloat x))
(defn out [x] (apply println x))
(def data-flow (csv/read-csv (BufferedReader. *in*) :separator \;))
(defn -main
  "docstring"
  [method step & other]
  (println "GOVNO")
  (if (= method "approximate")
    (do
      (loop [flow-iterator data-flow
             prev-point nil]
        (if (nil? prev-point)
          ()
          (do
            (let [x1 (to-float (first prev-point)) y1 (to-float (second prev-point))
                  x2 (to-float (first (first flow-iterator))) y2 (to-float (second (first flow-iterator)))]
              (out (map #(str "\t" (format "%.3f" %1) ";" (format "%.3f" %2) "\n")  (range x1 x2 (to-float step)) (map #(+ (/ (- (* x2 y1) (* x1 y2)) (- x2 x1)) (* % (/ (- y2 y1) (- x2 x1)))) (range x1 x2 (to-float step)))))
              )
            ))
        (recur (next flow-iterator) (first flow-iterator)))
      )


    (do
      (def quantity (first other))
      (println quantity)
      )
    )
  )
