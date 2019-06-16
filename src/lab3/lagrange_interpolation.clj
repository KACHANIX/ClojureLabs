(ns lab3.lagrange-interpolation)


(defn factors
  [x
   x-to-eliminate
   x-values-nodes]
  (map #(/ (- x %) (- x-to-eliminate %)) (filter #(not= % x-to-eliminate) x-values-nodes))
  )

(defn basic-pol-coll
  [x
   x-values-nodes]
  (map #(reduce * 1 (factors x % x-values-nodes)) x-values-nodes)
  )

(defn generate-lagrange-function
  [x-values-nodes
   y-values-nodes]
  (fn [x]
    (->> y-values-nodes
         (map * (basic-pol-coll x x-values-nodes))
         (reduce +))))

(defn lagrange
  [x-start x-end
   step data-flow
   min-points]
  (loop [prev-pos 0
         curr-pos (- min-points 1)
         curr-x-to-interpolate x-start]
    (let [prev-point (nth data-flow prev-pos)
          curr-point (nth data-flow curr-pos)
          y-values-nodes (->> data-flow
                              (drop prev-pos)
                              (take min-points)
                              (map #(second %)))
          x-values-nodes (->> data-flow
                              (drop prev-pos)
                              (take min-points)
                              (map #(first %)))
          curr-point-x (/ (+ (first prev-point) (first curr-point)) 2)
          f (generate-lagrange-function x-values-nodes y-values-nodes)]
      (if (and
            (<= curr-x-to-interpolate curr-point-x)
            (<= curr-x-to-interpolate x-end))
        (do
          (println (str (format "%.3f" curr-x-to-interpolate) ";" (format "%.3f" (f curr-x-to-interpolate))))
          (recur prev-pos curr-pos (+ curr-x-to-interpolate step)))
        (recur (inc prev-pos) (inc curr-pos) curr-x-to-interpolate)))))