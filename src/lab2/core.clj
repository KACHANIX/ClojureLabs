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


(defn put [hash-table key value]
  (if (= (count hash-table) (count (filter not-nil? hash-table)))
    "Hash-map is full!"
    (loop [index (mod (hash-sum key) (count hash-table))]
      (if (nil? (nth hash-table index))
        (do
          "Element is successfully added!"
          (assoc hash-table index (struct hash-map-element key value))
          )
        (if (= (:key (nth hash-table index)) key)
          (do
            "Element is successfully added!"
            (assoc hash-table index (struct hash-map-element key value)))
          (recur (mod (+ 1 index) (count hash-table))))))))

(defn get-by-key [hash-table key]
  (loop [index (mod (hash-sum key) (count hash-table)) iterations-count 0]
    (if (and (not= key (:key (nth hash-table index)))
             (not-nil? (nth hash-table index))
             (not= iterations-count (count hash-table)))
      (recur (mod (+ 1 index) (count hash-table)) (inc iterations-count))
      (if (or (nil? (nth hash-table index))
              (= iterations-count (count hash-table)))
        nil
        (:value (nth hash-table index))))))

(defn remove-by-key [hash-table key]
  (loop [index (mod (hash-sum key) (count hash-table)) iterations-count 0]
    (if (and (not= key (:key (nth hash-table index)))
             (not-nil? (nth hash-table index))
             (not= iterations-count (count hash-table)))
      (recur (mod (+ 1 index) (count hash-table)) (inc iterations-count))
      (if (or (nil? (nth hash-table index))
              (= iterations-count (count hash-table)))
        (str key " is not found!")
        (do
          (str key " is successfully removed")
          (assoc hash-table index nil))))))

(defn map-hash-table-elements [hash-table f]
  (vec (map #(if (not-nil? %)
               (hash-map :key (:key %),
                         :value (f (:value %))))
            hash-table)))

(defn fold-left [hash-table f & initial-value]
  (if (= initial-value nil)
    (reduce f (map #(:value %) (filter not-nil? hash-table)))
    (reduce f (first initial-value) (map #(:value %)
                                         (filter not-nil? hash-table)))))

(defn fold-right [hash-table f & initial-value]
  (if (= initial-value nil)
    (reduce f (map #(:value %) (reverse (filter not-nil? hash-table))))
    (reduce f (first initial-value)
            (map #(:value %) (reverse (filter not-nil? hash-table))))))

(defn filter-hash-table [hash-table pred]
  (vec (map #(if (nil? %) nil
                          (if (pred (:value %)) % nil)) hash-table)))

(defn merge-hash-tables [hash-table-1 hash-table-2]
  (if (= hash-table-1 hash-table-2)
    hash-table-1
    (if (nil? hash-table-2)
      hash-table-1
      (if (nil? hash-table-1)
        hash-table-2
        (loop [iter 0
               filtered-hash-table-2 hash-table-2
               merged-hash-table (vec (repeat (* 10 (+ (count hash-table-1)
                                                 (count hash-table-2))) nil))]
          (if (= iter (count hash-table-1))
            (do
              (loop [left-hash-table-2-elements (vec (filter not-nil? filtered-hash-table-2))
                     output-merged-hash-table merged-hash-table]
                (if (= 0 (count left-hash-table-2-elements))
                  output-merged-hash-table
                  (recur (rest left-hash-table-2-elements)
                         (put output-merged-hash-table (:key (first left-hash-table-2-elements))
                              (:value (first left-hash-table-2-elements)))))))
            (if (nil? (nth hash-table-1 iter))
              (recur (inc iter) filtered-hash-table-2 merged-hash-table)
              (do
                (if (nil? (get-by-key hash-table-2
                                      (:key (nth hash-table-1 iter))))
                  (recur (inc iter) filtered-hash-table-2
                         (put merged-hash-table (:key (nth hash-table-1 iter)) (:value (nth hash-table-1 iter))))
                  (do
                    (if (= 1 (resolver (nth hash-table-1 iter)
                                       (struct hash-map-element
                                               (:key (nth hash-table-1 iter))
                                               (get-by-key hash-table-2
                                                           (:key (nth hash-table-1 iter))))))
                      (recur (inc iter)
                             (filter #(not (=
                                             (:key (nth hash-table-1 iter))
                                             (:key %))) filtered-hash-table-2)
                             (put merged-hash-table (:key (nth hash-table-1 iter)) (:value (nth hash-table-1 iter))))

                      (recur (inc iter)
                             (filter #(not (=
                                             (:key (nth hash-table-1 iter))
                                             (:key %))) filtered-hash-table-2)
                             (put merged-hash-table
                                          (:key (nth hash-table-1 iter))
                                          (get-by-key hash-table-2 (:key (nth hash-table-1 iter))))))))))))))))


;(def a (vec (repeat 5 nil)))
;(println (filter-hash-table (put (put a (struct hash-map-element "asd" 123))
;                          (struct hash-map-element "dsa" 124)) even?))



;
;(def a (vec (repeat 5 nil)))
;(def b (vec (repeat 5 nil)))
;(println (merge-hash-tables (put (put a "asd" 123) "dsa" 124)
;                            (put (put b "asd" 320) "dsb" 421)))
;(filter-hash-table a even?)
;(println (get-elements a))
;(remove-by-key a "asd")
;(println(get-by-key a "asd"))