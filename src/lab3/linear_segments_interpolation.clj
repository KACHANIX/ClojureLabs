(ns lab3.linear-segments-interpolation)


(defn generate-linear-segments-function
  [a b]
  (let [x1 (first a)
        x2 (first b)
        y1 (second a)
        y2 (second b)
        k (/ (- y2 y1) (- x2 x1))
        b (/ (- (* x2 y1) (* x1 y2)) (- x2 x1))]

    (fn [x] (+ b (* x k)))))

(defn linear-segments
  [x-start x-end
   step data-flow]
  (loop [prev-pos 0
         curr-pos 1
         curr-x-to-interpolate x-start]
    (let [prev-point (nth data-flow prev-pos)
          curr-point (nth data-flow curr-pos)
          curr-point-x (first (nth data-flow curr-pos))
          f   (generate-linear-segments-function prev-point curr-point)]
      (if (and
            (<= curr-x-to-interpolate curr-point-x)
            (<= curr-x-to-interpolate x-end))
        (do
          (println (str (format "%.3f" curr-x-to-interpolate) ";" (format "%.3f" (f curr-x-to-interpolate))))
          (recur prev-pos
                 curr-pos
                 (+ curr-x-to-interpolate step)))
        (recur (inc prev-pos)
               (inc curr-pos)
               curr-x-to-interpolate)))))