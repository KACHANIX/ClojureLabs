(ns lab3.core
  (:import (java.io BufferedReader)))



(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])


(defn approximate
  [data-flow
   step]
  (loop [flow-iterator data-flow
         prev-point nil]
    (if (nil? prev-point)
      ()
      (do
        (let [x1 (Float/parseFloat (first prev-point))
              y1 (Float/parseFloat (second prev-point))
              x2 (Float/parseFloat (first (first flow-iterator)))
              y2 (Float/parseFloat (second (first flow-iterator)))]
          (println "chlen")
          (apply println (map #(str "\t" (format "%.1f" %1) ";" (format "%.1f" %2) "\n") (map #(+ (/ (- (* x1 y1) (* x2 y1)) (- x1 x2)) (* % (/ (- y1 y2) (- x1 x2)))) (range x1 x2 step)) (range x1 x2 step)))
          )
        ))
    (recur (next flow-iterator) (first flow-iterator)))
  )

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
            (let [x1 (Float/parseFloat (first prev-point))
                  y1 (Float/parseFloat (second prev-point))
                  x2 (Float/parseFloat (first (first flow-iterator)))
                  y2 (Float/parseFloat (second (first flow-iterator)))]
              (apply println (map #(str "\t" (format "%.3f" %1) ";" (format "%.3f" %2) "\n")  (range x1 x2 (Float/parseFloat step)) (map #(+ (/ (- (* x2 y1) (* x1 y2)) (- x2 x1)) (* % (/ (- y2 y1) (- x2 x1)))) (range x1 x2 (Float/parseFloat step)))))
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
