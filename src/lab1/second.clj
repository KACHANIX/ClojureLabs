(ns lab1.second)


"Решение 2 - ксеноморфное с рекурсией"
(defn get_lcm []
  (loop [N 0]
    (println N)
    (if (and (= 0 (mod N 1))
             (= 0 (mod N 2))
             (= 0 (mod N 3))
             (= 0 (mod N 4))
             (= 0 (mod N 5))
             (= 0 (mod N 6))
             (= 0 (mod N 7))
             (= 0 (mod N 8))
             (= 0 (mod N 9))
             (= 0 (mod N 10))
             (= 0 (mod N 11))
             (= 0 (mod N 12))
             (= 0 (mod N 13))
             (= 0 (mod N 14))
             (= 0 (mod N 15))
             (= 0 (mod N 16))
             (= 0 (mod N 17))
             (= 0 (mod N 18))
             (= 0 (mod N 19))
             (= 0 (mod N 20))
             (not (= 0 N)))
      N
      (recur (+ N 20)))
    )
  )

(get_lcm)