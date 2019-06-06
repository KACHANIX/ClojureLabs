(ns lab2.tests
  (:use clojure.test))

(use 'lab2.core)

;;;Property based tests




;Associative property of merge operation
;(a + b) + c = a + (b + c)
(def a (->hash-map-type (vec (repeat 5 nil))))
(def b (->hash-map-type (vec (repeat 5 nil))))
(def c (->hash-map-type (vec (repeat 5 nil))))

(put a {:key "A" :value 1})
(put a {:key "ASD" :value 1})
(put b {:key "B" :value 2})
(put b {:key "BSD" :value 2})
(put c {:key "C" :value 3})
(put c {:key "CSD" :value 3})



(deftest is-associative
  (is (=
        (do
          (merge-hash-tables a b) (merge-hash-tables a c)
          a)
        (do
          (merge-hash-tables b c) (merge-hash-tables a b)
          a))))


;Are hash-maps unqiue
;a + a = a
(deftest is-equal
  (is (=
        (do (merge-hash-tables a a)
            a)
        a)))


;Is there a unit
;aE = a = Ea

(def nil-map (->hash-map-type nil))
(deftest is-unit
  (is (=
        (do
          (merge-hash-tables a nil-map) (get-elements a))
        (get-elements a)
        (do
          (merge-hash-tables nil-map a)
          (get-elements nil-map))
        )))




;;;Unit test (all functions)
(def unit-1 (->hash-map-type (vec (repeat 5 nil))))
(def unit-2 (->hash-map-type (vec (repeat 5 nil))))
(deftest put-and-get-test
  (is (=
        (do
          (put unit-1 {:key "firstkey" :value 123})
          (get-by-key unit-1 "firstkey"))
        123)))
;(println (str "elements: " (get-elements unit-1)))

(deftest map-hash-table-elements-test
  (is (=
        (do
          (put unit-1 {:key "firstkey" :value 123})

          (map-hash-table-elements unit-1 inc)
          (get-by-key unit-1 "firstkey"))
        124)))

(deftest filter-hash-table-test
  (is (=
        (do
          (put unit-1 {:key "firstkey" :value 123})
          (put unit-1 {:key "secondkey" :value 120})
          (filter-hash-table unit-1 even?)
          (get-by-key unit-1 "firstkey"))
        nil)))

(deftest fold-left-test
  (is (=
        (do
          (put unit-1 {:key "firstkey" :value 123})
          (put unit-1 {:key "secondkey" :value 120})
          (fold-left unit-1 +)
          )
        243)))

(deftest merge-hash-tables-test
  (is (=
        (do
          (put unit-1 {:key "firstkey" :value 123})
          (put unit-1 {:key "secondkey" :value 120})
          (put unit-2 {:key "unit2-element" :value 45})
          (merge-hash-tables unit-1 unit-2)
          (get-by-key unit-1 "unit2-element"))
        45)))
(deftest fold-right-test
  (is (=
        (fold-right unit-1 -)
        -198)))
(deftest remove-by-key-test
  (is (=
        (do
          (put unit-1 {:key "firstkey" :value 123})

          (remove-by-key unit-1 "firstkey")
          (get-by-key unit-1 "firstkey"))
        nil)))


(run-tests)
