(ns lab3.io
  (:require [clojure.data.csv :as csv])
  (:import (java.io BufferedReader)))

(defn read-points
  "Returns LazySeq of stdin"
  []
  (map #(vec [(Float/parseFloat (first %))
              (Float/parseFloat (second %))])
       (csv/read-csv (BufferedReader. *in*) :separator \;)))