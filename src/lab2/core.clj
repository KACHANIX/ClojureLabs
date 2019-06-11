(ns lab2.core)

(defstruct hash-map-element :key :value)

(defn resolver [map-1 map-2]
  (if (= (:value map-1) (:value map-2))
    1
    (if (>
          (reduce + (map int (str (:value map-1))))
          (reduce + (map int (str (:value map-2))))) 1 2)))

(def not-nil? (complement nil?))

(defn hash-sum [input]
  (let [key (map int (str input))]
    (loop [N 0 sum 0]
      (if (not (= N (count key)))
        (recur (inc N) (+ sum (* (inc N) (nth key N))))
        sum))))


(defn put [elements new-element]
  (if (= (count elements) (count (filter not-nil? elements)))
    "Hash-map is full!"
    (loop [index (mod (hash-sum (:key new-element)) (count elements))]
      (if (nil? (nth elements index))
        (do
          "Element is successfully added!"
          (assoc elements index new-element)
          )
        (if (= (:key (nth elements index)) (:key new-element))
          (do
            "Element is successfully added!"
            (assoc elements index new-element))
          (recur (mod (+ 1 index) (count elements))))))))

(defn get-by-key [elements key]
  (loop [index (mod (hash-sum key) (count elements)) iterations-count 0]
    (if (and (not= key (:key (nth elements index)))
             (not-nil? (nth elements index))
             (not= iterations-count (count elements)))
      (recur (mod (+ 1 index) (count elements)) (inc iterations-count))
      (if (or (nil? (nth elements index))
              (= iterations-count (count elements)))
        nil
        (:value (nth elements index))))))

(defn remove-by-key [elements key]
  (loop [index (mod (hash-sum key) (count elements)) iterations-count 0]
    (if (and (not= key (:key (nth elements index)))
             (not-nil? (nth elements index))
             (not= iterations-count (count elements)))
      (recur (mod (+ 1 index) (count elements)) (inc iterations-count))
      (if (or (nil? (nth elements index))
              (= iterations-count (count elements)))
        (str key " is not found!")
        (do
          (str key " is successfully removed")
          (assoc elements index nil))))))

(defn map-hash-table-elements [elements f]
  (vec (map #(if (not-nil? %)
               (hash-map :key (:key %),
                         :value (f (:value %))))
            elements)))

(defn fold-left [elements f & initial-value]
  (if (= initial-value nil)
    (reduce f (map #(:value %) (filter not-nil? elements)))
    (reduce f (first initial-value) (map #(:value %)
                                         (filter not-nil? elements)))))

(defn fold-right [elements f & initial-value]
  (if (= initial-value nil)
    (reduce f (map #(:value %) (reverse (filter not-nil? elements))))
    (reduce f (first initial-value)
            (map #(:value %) (reverse (filter not-nil? elements))))))

(defn filter-hash-table [elements pred]
  (vec (map #(if (nil? %) nil
                          (if (pred (:value %)) % nil)) elements)))

(defn merge-hash-tables [hash-table-1 hash-table-2]
  (if (= hash-table-1 hash-table-2)
    hash-table-1
    (if (nil? hash-table-2)
      hash-table-1
      (if (nil? hash-table-1)
        hash-table-2
        (loop [iter 0
               filtered-hash-table-2 hash-table-2
               merged-hash-table (vec (repeat (+ (count hash-table-1)
                                                 (count hash-table-2)) nil))]
          (if (= iter (count hash-table-1))
            (do
              (loop [left-hash-table-2-elements (vec (filter not-nil? filtered-hash-table-2))
                     output-merged-hash-table merged-hash-table]
                (if (= 0 (count left-hash-table-2-elements))
                  output-merged-hash-table
                  (recur (rest left-hash-table-2-elements)
                         (put output-merged-hash-table (first left-hash-table-2-elements))))))
            (if (nil? (nth hash-table-1 iter))
              (recur (inc iter) filtered-hash-table-2 merged-hash-table)
              (do
                (if (nil? (get-by-key hash-table-2
                                      (:key (nth hash-table-1 iter))))
                  (recur (inc iter) filtered-hash-table-2
                         (put merged-hash-table (nth hash-table-1 iter)))
                  (do
                    (if (= 1 (resolver (nth hash-table-1 iter)
                                       (struct hash-map-element
                                               (:key (nth hash-table-1 iter))
                                               (get-by-key hash-table-2
                                                           (:key hash-table-1 iter)))))
                      (recur (inc iter)
                             (filter #(not (=
                                             (:key (nth hash-table-1 iter))
                                             (:key %))) filtered-hash-table-2)
                             (put merged-hash-table (nth hash-table-1 iter)))

                      (recur (inc iter)
                             (filter #(not (=
                                             (:key (nth hash-table-1 iter))
                                             (:key %))) filtered-hash-table-2)
                             (put merged-hash-table
                                  (struct hash-map-element
                                          (:key (nth hash-table-1 iter))
                                          (get-by-key hash-table-2 (:key (nth hash-table-1 iter)))))))))))))))))


;(def a (vec (repeat 5 nil)))
;(println (filter-hash-table (put (put a (struct hash-map-element "asd" 123))
;                          (struct hash-map-element "dsa" 124)) even?))




(def a (vec (repeat 5 nil)))
(def b (vec (repeat 5 nil)))
(println (merge-hash-tables (put (put a (struct hash-map-element "asd" 123))
                                 (struct hash-map-element"dsa" 124))
                            (put (put b (struct hash-map-element "bsd" 321))
                                 (struct hash-map-element "dsb" 421))) )
;(filter-hash-table a even?)
;(println (get-elements a))
;(remove-by-key a "asd")
;(println(get-by-key a "asd"))