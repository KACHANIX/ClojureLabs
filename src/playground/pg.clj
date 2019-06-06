(ns playground.pg)
(require '[clojure.core.reducers :as r])
;
;
;(defprotocol Hasher
;  (hash-and-add [this new-element-key new-element-value])
;  (print-it [this])
;  (map-it [this f] [this f x])
;  (left-fold-it [this f] [this f x])
;  (right-fold-it [this f] [this f x])
;
;  (delete-it [])
;  )
;
;(deftype my-hash-map [^{:volatile-mutable true} elements]
;  Hasher
;  (hash-and-add [this new-element-key new-element-value]
;    (set! elements
;          (assoc elements (mod (hash new-element-key) (count elements)) new-element-value)))
;  (print-it [this] (println (str elements)))
;
;  (map-it [this f ]
;    (set! elements (vec (map #(if (= nil %) nil (f %)) elements))))
;  (map-it [this f x]
;    (set! elements (vec (map #(if (= nil %) nil (f % x)) elements))))
;
;  (left-fold-it [this f]
;    (reduce f (remove nil? elements)))
;  (left-fold-it [this f x]
;    (reduce f x (remove nil? elements)))
;
;  (right-fold-it [this f]
;    (reduce f (reverse (remove nil? elements))))
;  (right-fold-it [this f x]
;    (reduce f x (reverse (remove nil? elements))))
;  )
;
;
;
;(def am (my-hash-map. (vec (replicate 10 nil))))
;
;(hash-and-add am "govno" 5)
;(hash-and-add am "ad" 6)
;(print-it am)
;
;(map-it am * 5)
;(print-it am)
;(println (left-fold-it am *))
;(println (left-fold-it am * 3))

;














(defn resolver [map-1 map-2]
  (if (= (:value map-1) (:value map-2))
    1
    (if (> (reduce + (map int (str (:value map-1)))) (reduce + (map int (str (:value map-2))))) 1 2)))

;(def count elements) 5)
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
  (filter-hash-table [this pred])
  )

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
            (if (even? (:value %)) % nil)) elements))))

  (merge-hash-tables [this hash-table-2]
    (if (= (get-elements this) (get-elements hash-table-2))
      ()
      (if (nil? hash-table-2)
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

;(def a (hash-map-type. (vec (repeat 5 nil))))
;(def b (hash-map-type. (vec (repeat 5 nil))))
;
;(put a {:key "B" :value 1})
;(put a {:key "BSD" :value 323})
;(put b {:key "B" :value 2})
;(put b {:key "BSD" :value 323})
;(merge-hash-tables a b)
;(println (get-elements a))
;(filter-hash-table a even?)
;(println (get-elements a))

;Associative property of merge operation
;(a + b) + c = a + (b + c)










;
(def a (hash-map-type. (vec (repeat 5 nil))))
(put a {:key "asd" :value 123})
(remove-by-key a "asd")
(println(get-by-key a "asd"))
;(put a {:key "B" :value 0})
;(def b (hash-map-type. (vec (repeat 5 nil))))
;(put b {:key "B" :value 1})
;(put b {:key "BSD" :value 323})
;(println (get-elements a))
;(println (get-elements b))
;(merge-hash-tables a b)
;(println (get-elements a))
;
;
;
;(def c (hash-map-type. (vec (repeat 5 nil))))
;(put c {:key "asd" :value 123})
;(put c {:key "B" :value 0})
;(def d (hash-map-type. (vec (repeat 5 nil))))
;(put d {:key "B" :value 1})
;(put d {:key "BSD" :value 323})
;(merge-hash-tables d (hash-map-type. (vec (repeat 1 nil))))
;(println "\n\n\n")
;(println (get-elements d))

