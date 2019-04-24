(ns lab1.first)


"Решение 1 - человеческое, основное"
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
(println (reduce #(lcm %1 %2) (range 1 21)))
