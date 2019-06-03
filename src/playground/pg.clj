(ns playground.pg)
(require '[clojure.core.reducers :as r])


(defprotocol Hasher
  (hash-and-add [this new-element-key new-element-value])
  (print-it [this])
  (map-it [this f] [this f x])
  (left-fold-it [this f] [this f x])
  (right-fold-it [this f] [this f x])

  (delete-it [])
  )

(deftype my-hash-map [^{:volatile-mutable true} elements]
  Hasher
  (hash-and-add [this new-element-key new-element-value]
    (set! elements
          (assoc elements (mod (hash new-element-key) (count elements)) new-element-value)))
  (print-it [this] (println (str elements)))

  (map-it [this f ]
    (set! elements (vec (map #(if (= nil %) nil (f %)) elements))))
  (map-it [this f x]
    (set! elements (vec (map #(if (= nil %) nil (f % x)) elements))))

  (left-fold-it [this f]
    (reduce f (remove nil? elements)))
  (left-fold-it [this f x]
    (reduce f x (remove nil? elements)))

  (right-fold-it [this f]
    (reduce f (reverse (remove nil? elements))))
  (right-fold-it [this f x]
    (reduce f x (reverse (remove nil? elements))))
  )



(def am (my-hash-map. (vec (replicate 10 nil))))

(hash-and-add am "govno" 5)
(hash-and-add am "ad" 6)
(print-it am)

(map-it am * 5)
(print-it am)
(println (left-fold-it am *))
(println (left-fold-it am * 3))


