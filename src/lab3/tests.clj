(ns lab3.tests
  (:use clojure.test))

(use 'lab3.core)

(defn f1 [x]
  (+ (* 3.0 x x) 3.5 (- (* 5.0 x x x (/ 1.0 12.0))) (- (* 61.0 x (/ 1.0 12.0)))))

(defn f2 [x]
  (+ (* 47 x x x x (/ 1 2016)) (- (* 25 x x x (/ 1 56))) (* 5233 x x (/ 1 2016)) (- (* 337 x (/ 1 84))) (/ 159 56)))

(deftest lagrange-1
  (is (=
        (map #(float (f1 %)) (range 1.0 5.0 0.25))
        (map #(float %) (lagrange 1 5 '(1.0 2.0 3.0 5.0) '(1.0 2.0 4.0 1.0) 0.25)))))


(deftest lagrange-2
  (is (=
        (map #(float (f2 %)) (range 1.0 10.0 0.5))
        (map #(float %) (lagrange 1 10 '(1.0 2.0 3.0 6.0 10.0) '(1.0 2.0 4.0 6.0 9.0) 0.5)))))

(run-tests)
