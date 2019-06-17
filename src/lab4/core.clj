(ns lab4.core)
(require '[riddley.walk :as rw])


(defn recur-statement? [expr]
  (and (seq? expr)
       (= (first expr) 'my-recur)))


(defn recur-expression [loop-finished-sym loop-bindings-sym node]
  `(do
     (reset! ~loop-finished-sym false)
     (reset! ~loop-bindings-sym
             (vec (interleave (take-nth 2 @~loop-bindings-sym) (list ~@(rest node)))))
     nil))


(defmacro eblan
  []
  `(println "idi naxui"))


(defmacro my-loop
  [user-bindings & body]
  ;(println user-bindings)
  ;(println body)

  (let [finished (gensym "finished")
        bindings (gensym "binding")
        return-value (gensym "return-value")
        ;post-exp (gensym "postexp")

        ]
    `(let [~finished (atom false)
           ~bindings (atom '~user-bindings)
           ~return-value (atom nil)
           ~'break (delay (throw (ex-info nil {:type :break})))
           ~'continue (delay (throw (ex-info nil {:type :continue})))
           ;~post-exp (last '~@body)
           ]
       (try
         (while (not @~finished)

           (reset! ~finished true)
           (try
             (reset! ~return-value
                     (apply
                       (fn ~(vec (take-nth 2 user-bindings))
                         ~@(rw/walk-exprs recur-statement? (partial recur-expression finished bindings) body))
                       (take-nth 2 (rest @~bindings))
                       ))
             @~return-value

             (catch Exception e#
               ;(reset! ~bindings [(first @~bindings) (inc (second @~bindings))])
               (when-not (= :continue (:type (ex-data e#)))
                 (throw e#)
                       )
               (reset! ~finished false)
               )
             )
           ;(println @~finished)
           )
         (catch Exception e#
           (println "ETO BREAK")
           (when-not (= :break (:type (ex-data e#)))
             (throw e#))))))
  )

(defmacro for-loop
  [[symbol init-value check-exp post-exp] & body]
  (let [body-tmp (concat body (list (list 'my-recur post-exp)))]
    `(my-loop [~symbol ~init-value]
              (if ~check-exp
                (do
                  ~@body-tmp
                  )))))

(println (macroexpand
           (for-loop [i 1 (< i 8) (inc i)]

                     (if (= i 3) @continue)
                     (println i)
                     )))
;(println (macroexpand (for-loop [i 1 (< i 3) (inc i)] (eblan))))







;
;(defmacro my-loop [user-bindings & body]
;  (let [finished (gensym "finished")
;        bindings (gensym "bindings")
;        return-value (gensym "return-value")]
;    `(let [~finished (atom false)
;           ~bindings (atom '~user-bindings)
;           ~return-value (atom nil)]
;       (while (not @~finished)
;         (reset! ~finished true)
;         (reset! ~return-value
;                 (apply
;                   (fn ~(vec (take-nth 2 user-bindings))
;                     ~@(riddley.walk/walk-exprs recur-statement? (partial recur-expression finished bindings) body))
;                   (take-nth 2 (rest @~bindings))))
;         @~return-value))))
(defmacro for-loop [[s i c p] & body]
  `(let [~'break (delay (throw (ex-info nil {:type :break})))
         ~'continue (delay (throw (ex-info nil {:type :continue})))]
     (try
       (loop [~s ~i]
         (when ~c
           (try
             ~@body
             (catch Exception e#
               (when-not (= :continue (:type (ex-data e#)))
                 (throw e#))))
           (recur ~p)))
       (catch Exception e#
         (when-not (= :break (:type (ex-data e#)))
           (throw e#))))))