(ns lab3.tests
  (:use clojure.test
        lab3.linear-segments-interpolation))

(deftest linear-segments-test
  (let [a [-1 1]
        b [1 3]
        f (generate-linear-segments-function a b)
        x-values [-1.000 -0.750 -0.500 -0.250 0.000 0.250 0.500 0.750 1.000]
        actual-y-values [1.000 1.250 1.500 1.750 2.000 2.250 2.500 2.750 3.000]
        tested-values (map #(conj [] (f %)) x-values)]
    (loop [iter 0]
      (if (not (= iter (count actual-y-values)))
        (is (=
              (nth actual-y-values iter)
              (first (nth tested-values iter))))))))

(deftest lagrange-interpolation-test
  (let [x-values-to-gen [1 2 3 4 5]
        y-values-to-gen [2 3 4 5 6]
        f (generate-linear-segments-function x-values-to-gen y-values-to-gen)
        x-values [-1.000 -0.750 -0.500 -0.250 0.000 0.250 0.500 0.750 1.000]
        actual-y-values [0.000 0.250 0.500 0.750 1.000 1.250 1.500 1.750 2.000]
        tested-values (map #(conj [] (f %)) x-values)]
    (loop [iter 0]
      (if (not (= iter (count actual-y-values)))
        (is (=
              (nth actual-y-values iter)
              (first (nth tested-values iter))))))))

(run-tests)
;
;(def a (generate-linear-segments-function [1 2 3 4 5]
;                                          [2 3 4 5 6]))
;(println(map #(conj [] (a %)) [-1.000 -0.750 -0.500 -0.250 0.000 0.250 0.500 0.750 1.000]))