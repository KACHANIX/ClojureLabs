(ns lab1.second)

(defmacro is-divided [N quantity]
  (println N "    " quantity)
  (let [A (map inc (take quantity (range)))]
    (if (= A (filter #(= 0 (mod N %)) (map inc (take quantity (range))))) true false)
    )
  )

;(defmacro check [n x] (println "zal") `(and ~@(map (fn [e] `(= 0 (mod ~x ~e))) (map inc (take n (range))))))
(defn check [n x] `(and ~@(map (fn [e] `(= 0 (mod ~x ~e))) (map inc (take n (range))))))

"Решение 2 - ксеноморфное с рекурсией"
(defn get_lcm [quantity]
  (loop [N quantity]
    (if `(check quantity N)
      (println (check quantity N))
      (do
        (println N)
        (recur (+ N quantity))
        )
      )
    )
  )

(get_lcm 10)
