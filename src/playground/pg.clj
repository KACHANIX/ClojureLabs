(ns playground.pg)
(require '[clojure.core.reducers :as r])

(def a (map inc))

(println (transduce a conj (range 5)))
(let*
  [finished399 (clojure.core/atom false)
   binding400 (clojure.core/atom (quote [i 10 (map inc) +]))
   return-value401 (clojure.core/atom nil)
   break (clojure.core/delay
           (throw (clojure.core/ex-info nil {:type :break})))
   continue (clojure.core/delay
              (throw (clojure.core/ex-info nil {:type :continue})))
   postexp402 (clojure.core/atom nil)
   all-counters403 (clojure.core/atom
                     (clojure.core/conj
                       []
                       (clojure.core/second (clojure.core/deref binding400))))
   arg1404 (quote (map inc))
   arg2405 (quote +)]

  (clojure.core/loop
    [reducing-body__365__auto__ (quote ((if (> i 2)
                                          (do (if (= i 5)
                                                (clojure.core/deref continue))
                                              (println i)
                                              (my-recur (dec i))))))]
    (if (clojure.core/=
          (clojure.core/str (clojure.core/first reducing-body__365__auto__))
          my-recur)
      (clojure.core/reset! postexp402
                           (clojure.core/last reducing-body__365__auto__))
      (recur (clojure.core/last reducing-body__365__auto__))))
  (try
    (clojure.core/while
      (clojure.core/not (clojure.core/deref finished399))
      (clojure.core/reset! finished399 true)
      (try
        (clojure.core/reset! return-value401
                             (clojure.core/apply
                               (clojure.core/fn [i]
                                 (if (. clojure.lang.Numbers
                                        (clojure.core/gt i 2))
                                   (do
                                     (if
                                       (. clojure.lang.Util
                                          clojure.core/equiv i 5)
                                       (clojure.core/deref continue))
                                     (println i)
                                     (do
                                       (clojure.core/reset! finished399 false)
                                       (clojure.core/reset!
                                         binding400
                                         (clojure.core/vec
                                           (clojure.core/interleave
                                             (clojure.core/take-nth
                                               2
                                               (clojure.core/deref binding400))
                                             (clojure.core/list (dec i)))))
                                       nil))))
                               (clojure.core/conj
                                 (quote ())
                                 (clojure.core/second
                                   (clojure.core/deref binding400)))))
        (clojure.core/reset!
          all-counters403
          (clojure.core/conj
            (clojure.core/deref all-counters403)
            (clojure.core/second (clojure.core/deref binding400))))
        (catch java.lang.Exception e__366__auto__
          (clojure.core/reset!
            binding400
            [(clojure.core/first (clojure.core/deref binding400))
             ((clojure.core/deref
                (clojure.core/resolve
                  (clojure.core/first (clojure.core/deref postexp402))))
               (clojure.core/second (clojure.core/deref binding400)))])
          (clojure.core/when-not
            (clojure.core/= :continue
                            (:type (clojure.core/ex-data e__366__auto__)))
            (throw e__366__auto__))
          (clojure.core/reset! finished399 false)
          (clojure.core/reset! all-counters403
                               (clojure.core/assoc
                                 (clojure.core/deref all-counters403)
                                 (clojure.core/dec
                                   (clojure.core/count
                                     (clojure.core/deref all-counters403)))
                                 ((clojure.core/deref
                                    (clojure.core/resolve
                                      (clojure.core/first
                                        (clojure.core/deref postexp402))))
                                   (clojure.core/last
                                     (clojure.core/deref all-counters403))))))))
    (catch java.lang.Exception e__366__auto__
      (clojure.core/when-not
        (clojure.core/= :break (:type (clojure.core/ex-data e__366__auto__)))
        (clojure.core/reset! all-counters403
                             (clojure.core/conj
                               (clojure.core/deref all-counters403)
                               0)))))
  (if (clojure.core/=
        (clojure.core/last (clojure.core/deref all-counters403))
        (clojure.core/last (clojure.core/drop-last
                             (clojure.core/deref all-counters403))))
    (clojure.core/reset! all-counters403
                         (clojure.core/vec
                           (clojure.core/drop-last
                             (clojure.core/drop-last
                               (clojure.core/deref all-counters403)))))
    (clojure.core/reset!
      all-counters403
      (clojure.core/vec
        (clojure.core/drop-last (clojure.core/deref all-counters403)))))
  (clojure.core/println all iteration elements
                        (clojure.core/deref all-counters403))
  (if (clojure.core/= nil arg1404)
    (clojure.core/deref all-counters403)
    (if (clojure.core/= (clojure.core/str arg1404) reduce)
      (clojure.core/reduce (clojure.core/resolve arg2405)
                           (clojure.core/deref all-counters403))
      (if (clojure.core/= arg2405 nil)
        (clojure.core/transduce (map inc) clojure.core/conj
                                (clojure.core/deref all-counters403))
        (clojure.core/transduce (map inc) (clojure.core/resolve arg2405)
                                (clojure.core/deref all-counters403))))))
