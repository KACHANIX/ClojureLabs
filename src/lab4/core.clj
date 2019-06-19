(ns lab4.core
  (:import (clojure.lang Symbol)))
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

(defmacro my-loop
  [user-bindings & body]
  (let [finished (gensym "finished")
        bindings (gensym "binding")
        return-value (gensym "return-value")
        post-exp (gensym "postexp")]
    `(let [~finished (atom false)
           ~bindings (atom '~user-bindings)
           ~return-value (atom nil)
           ~'break (delay (throw (ex-info nil {:type :break})))
           ~'continue (delay (throw (ex-info nil {:type :continue})))
           ~post-exp (atom nil)]
       (loop [reducing-body# '~body]
         (if (= (str (first reducing-body#)) "my-recur")
           (reset! ~post-exp (last reducing-body#))
           (recur (last reducing-body#))))
       (try
         (while (not @~finished)
           (reset! ~finished true)
           (try
             (reset! ~return-value
                     (apply
                       (fn ~(vec (take-nth 2 user-bindings))
                         ~@(rw/walk-exprs recur-statement?
                                          (partial recur-expression finished bindings) body))
                       (take-nth 2 (rest @~bindings))
                       ))
             @~return-value

             (catch Exception e#
               (reset! ~bindings [(first @~bindings) (@(resolve (first @~post-exp)) (second @~bindings))])
               (when-not (= :continue (:type (ex-data e#)))
                 (throw e#))
               (reset! ~finished false))))
         (catch Exception e#
           (when-not (= :break (:type (ex-data e#)))
             (throw e#)))))))




(defmacro for-loop
  [[symbol init-value check-exp post-exp] & body]
  (let [body-tmp (concat body (list (list 'my-recur post-exp)))]
    `(my-loop [~symbol ~init-value]
              (if ~check-exp
                (do
                  ~@body-tmp)))))


(defmacro for-loop-with-global-variable
  [[check-exp post-exp] & body]
  (let [body-tmp (concat body (list (list 'my-recur post-exp)))
        symbol# (second check-exp)
        init-value @(resolve (symbol symbol#))
        ]
    `(my-loop [~symbol# ~init-value]
              (if ~check-exp
                (do
                  ~@body-tmp)))))

(defmacro for-in-range
  [[symbol range] & body]
  (let [init-val (atom nil)
        post-exp (atom nil)
        check-exp (atom nil)]
    (if (= (count range) 2)
      (do
        (reset! init-val 0)
        (reset! post-exp `(inc ~symbol))
        (reset! check-exp `(< ~symbol ~(nth range 1)))
        )
      (do
        (reset! init-val (nth range 1))
        (reset! post-exp (if (> (nth range 1) (nth range 2))
                           `(dec ~symbol)
                           `(inc ~symbol)))
        (reset! check-exp (if (> (nth range 1) (nth range 2))
                            `(> ~symbol ~(nth range 2))
                            `(< ~symbol ~(nth range 2))))))
    (let [body-tmp (concat body (list (list 'my-recur @post-exp)))
          check-expression @check-exp
          initial-value @init-val]
      `(my-loop [~symbol ~initial-value]
                (if ~check-expression
                  (do
                    ~@body-tmp))))))


;(macroexpand
;           (for-in-range [i (range 2 8)]
;                         (if (= i 3) @continue)
;                         (if (= i 7) @break)
;                         (println i)))

;(def a 1)
; (macroexpand
;           (for-loop-with-global-variable [(< a 10) (inc a)]
;                         (if (= a 3) @continue)
;                         (if (= a 8) @break)
;                         (println a)))

 (macroexpand
           (for-loop [i 10 (> i 2) (dec i)]
                     (if (= i 3) @break)
                     (if (= i 5) @continue)
                     (println i)))
;