(ns lab1.third)


"Решение 3 - человеческое, основное c бесконечной последовательностью из (range)
   https://medium.com/@dawranliou/lazy-codes-infinite-sequences-in-python-and-clojure-80bba720b3a3
   Let’s take a look at Clojure. When the range function is called as a zero-arity function (without arguments,)
   it generates infinite sequence of numbers."
(defn gcd [a b]
  "Greatest common divider"
  (if (zero? b)
    a
    (recur b (mod a b))
    )
  )
(defn lcm [a b]
  "Lowest common multiple"
  (println a "   " b)
  (/ (* a b) (gcd a b))
  )

(defn third [quantity]
  (println (reduce #(lcm %1 %2) (map inc (take quantity (range)))))
  )
(third 20)