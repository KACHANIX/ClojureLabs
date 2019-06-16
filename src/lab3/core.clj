(ns lab3.core
  (:import (java.io BufferedReader)))

(require '[lab3.io :as io])

(use 'lab3.linear-segments-interpolation
     'lab3.lagrange-interpolation)

(defn -main
  "Main clj handler for linear segments & lagrange interpolations"
  [x-start-str
   x-end-str
   step-str
   method & min-points]
  (println x-start-str)
  (let [x-start (Float/parseFloat x-start-str)
        x-end (Float/parseFloat x-end-str)
        step (Float/parseFloat step-str)
        data-flow (io/read-points)]
    (if (= method "segments")
      (linear-segments x-start x-end step data-flow)
      (lagrange x-start x-end step data-flow (Long/parseLong (first min-points))))))
