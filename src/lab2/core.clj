(ns lab2.core)




(defn resolver [map-1 map-2]
  (if (= (:value map-1) (:value map-2))
    1
    (if (> (reduce + (map int (str (:value map-1)))) (reduce + (map int (str (:value map-2))))) 1 2)))

(def not-nil? (complement nil?))

(defn hash-sum [input]
  (let [key (map int (str input))]
    (loop [N 0 sum 0]
      (if (not (= N (count key)))
        (recur (inc N) (+ sum (* (inc N) (nth key N))))
        sum))))


(defprotocol hash-map-protocol
  (put [this new-element])
  (get-by-key [this key])
  (remove-by-key [this key])
  (map-hash-table-elements [this f])
  (fold-left [this f] [this f initial-value])
  (fold-right [this f] [this f initial-value])
  (merge-hash-tables [map1 map2])
  (get-elements [this])
  (filter-hash-table [this pred]))

(deftype hash-map-type [^:volatile-mutable elements]
  hash-map-protocol
  (put [this new-element]
    (if (= (count elements) (count (filter not-nil? elements)))
      "Hash-map is full!"
      (loop [index (mod (hash-sum (:key new-element)) (count elements))]
        (if (nil? (nth elements index))
          (do
            (set! elements (assoc elements index new-element))
            "Element is successfully added!")
          (if (= (:key (nth elements index)) (:key new-element))
            (do
              (set! elements (assoc elements index new-element))
              "Element is successfully added!")
            (recur (mod (+ 1 index) (count elements))))))))

  (get-by-key [this key]
    (loop [index (mod (hash-sum key) (count elements)) iterations-count 0]
      (if (and (not= key (:key (nth elements index)))
               (not-nil? (nth elements index))
               (not= iterations-count (count elements)))
        (recur (mod (+ 1 index) (count elements)) (inc iterations-count))
        (if (or (nil? (nth elements index))
                (= iterations-count (count elements)))
          ;(str key " is not found!")
          nil
          (:value (nth elements index))))))

  (remove-by-key [this key]
    (loop [index (mod (hash-sum key) (count elements)) iterations-count 0]
      (if (and (not= key (:key (nth elements index)))
               (not-nil? (nth elements index))
               (not= iterations-count (count elements)))
        (recur (mod (+ 1 index) (count elements)) (inc iterations-count))
        (if (or (nil? (nth elements index))
                (= iterations-count (count elements)))
          (str key " is not found!")
          (do
            (set! elements (assoc elements index nil))
            (str key " is successfully removed"))))))

  (map-hash-table-elements [this f]
    (set! elements (vec (map #(if (not-nil? %)
                                (hash-map :key (:key %),
                                          :value (f (:value %))))
                             elements))))

  (fold-left [this f]
    (reduce f (map #(:value %) (filter not-nil? elements))))

  (fold-left [this f initial-value]
    (reduce f initial-value (map #(:value %) (filter not-nil? elements))))

  (fold-right [this f]
    (reduce f (map #(:value %) (reverse (filter not-nil? elements)))))

  (fold-right [this f initial-value]
    (reduce f initial-value (map #(:value %) (reverse (filter not-nil? elements)))))

  (get-elements [this]
    elements)

  (filter-hash-table [this pred]
    (set! elements (vec (map #(if (nil? %)
                                nil
                                (if (pred (:value %)) % nil)) elements))))

  (merge-hash-tables [this hash-table-2]
    (if (= (get-elements this) (get-elements hash-table-2))
      ()
      (if (nil? (get-elements hash-table-2))
        ()
        (if (nil? elements)
          (set! elements (get-elements hash-table-2))
          (let [elements-of-hash-table-1 elements]
            (set! elements (vec (repeat (+ (count elements) (count (get-elements hash-table-2))) nil)))
            (loop [iter 0
                   filtered-elements-hash-table-2 (get-elements hash-table-2)]
              (if (= iter (count elements-of-hash-table-1))
                (doall (map #(put this %) (vec (filter not-nil? filtered-elements-hash-table-2))))
                (if (nil? (nth elements-of-hash-table-1 iter))
                  (recur (inc iter) filtered-elements-hash-table-2)
                  (do
                    (if (nil? (get-by-key hash-table-2 (:key (nth elements-of-hash-table-1 iter))))
                      (do
                        (put this (nth elements-of-hash-table-1 iter))
                        (recur (inc iter) filtered-elements-hash-table-2))
                      (do
                        (if (= 1 (resolver (nth elements-of-hash-table-1 iter)
                                           {:key (:key (nth elements-of-hash-table-1 iter)) :value (get-by-key hash-table-2 (:key (nth elements-of-hash-table-1 iter)))}))
                          (put this (nth elements-of-hash-table-1 iter))
                          (put this
                               {:key (:key (nth elements-of-hash-table-1 iter)) :value (get-by-key hash-table-2 (:key (nth elements-of-hash-table-1 iter)))}))
                        (recur (inc iter) (filter #(not (= (:key (nth elements-of-hash-table-1 iter)) (:key %))) filtered-elements-hash-table-2))))))))))))))

;
;(def a (hash-map-type. (vec (repeat 5 nil))))
;(put a {:key "asd" :value 124})
;(filter-hash-table a even?)
;(println (get-elements a))
;(remove-by-key a "asd")
;(println(get-by-key a "asd"))