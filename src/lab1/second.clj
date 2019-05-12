(ns lab1.second)

(defmacro is-divided [N quantity]
  (println N "    " quantity)
  (let [A (map inc (take quantity (range)))]
    (if (= A (filter #(= 0 (mod N %)) (map inc (take quantity (range))))) true false)
    )
  )

"Решение 2 - ксеноморфное с рекурсией"
(defn get_lcm [quantity]
    (loop [N 0]
      (if (and (macroexpand `(is-divided ~N ~quantity))
               (not (= 0 N)))
        N
        (recur (+ N 20)))))
(get_lcm 20)
