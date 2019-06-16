(ns lab2.tests
  (:use clojure.test
        lab2.core)
  (:require
    [clojure.test :refer :all]
    [clojure.test.check :as tc]
    [clojure.test.check.clojure-test :refer (defspec)]
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop]))

(defn gen-hash-map [quantity]
  (loop [generated-hash-map (vec (repeat quantity nil))
         iter 0]
    (if (not (even? iter))
      (recur generated-hash-map (inc iter))
      (if (= quantity iter)
        generated-hash-map
        (recur (put generated-hash-map
                    (gen/generate gen/string-ascii)
                    (gen/generate gen/int))
               (inc iter))))))


;;Property based tests
;Associative property of merge operation
;(a + b) + c = a + (b + c)

(defspec associativity-test 50
         (prop/for-all
           [d gen/int]
           (let [a (gen-hash-map 10)
                 b (gen-hash-map 10)
                 c (gen-hash-map 10)]
             (is (=
                   (merge-hash-tables (merge-hash-tables a b) c)
                   (merge-hash-tables a (merge-hash-tables b c)))))))




;Are hash-maps unqiue
;a + a = a

(defspec is-equal-test 100
         (prop/for-all
           [d gen/int]
           (let [a (gen-hash-map 10)]
             (is (=
                   (merge-hash-tables a a)
                   a)))))

;Is there a unit
;aE = a = Ea


(defspec is-unit 50
         (prop/for-all
           [d gen/int]
           (let [a (gen-hash-map 10)]
             (is (=
                   (merge-hash-tables a nil)
                   a
                   (merge-hash-tables nil a))))))

;;;Unit tests (all functions)
(def unit-1 (vec (repeat 5 nil)))
(def unit-2 (vec (repeat 5 nil)))

(deftest put-and-get-test
  (is (=
        (get-by-key (put unit-1 "firstkey" 123) "firstkey")
        123)))
;(println (str "elements: " (get-elements unit-1)))

(deftest map-hash-table-elements-test
  (is (=
        (get-by-key (map-hash-table-elements (put unit-1 "firstkey" 123) inc) "firstkey")
        124)))

(deftest filter-hash-table-test
  (is (=
        (get-by-key (filter-hash-table (put (put unit-1 "firstkey" 123) "secondkey" 120) even?) "firstkey")
        nil)))

(deftest fold-left-test
  (is (=
        (fold-left (put (put unit-1 "firstkey" 123) "secondkey" 120) +)
        243)))

(deftest merge-hash-tables-test
  (is (=

        (get-by-key (merge-hash-tables (put (put unit-1 "firstkey" 123) "secondkey" 120)
                                       (put unit-2 "unit2-element" 45))
                    "unit2-element")
        45)))

(deftest fold-right-test
  (is (=
        (fold-right (put (put unit-1 "firstkey" 123) "secondkey" 120) -)
        -3)))

(deftest remove-by-key-test
  (is (=
        (get-by-key (remove-by-key (put unit-1 "firstkey" 123) "firstkey") "firstkey")
        nil)))

(run-tests)
