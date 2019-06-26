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
             (vec (interleave (take-nth 2 @~loop-bindings-sym)
                              (list ~@(rest node)))))
     nil))

(defmacro my-loop
  [user-bindings & body]
  (let [finished (gensym "finished")
        bindings (gensym "binding")
        return-value (gensym "return-value")                ;Создать уникальные
        post-exp (gensym "postexp")                         ;названия для
        all-iterators (gensym "all-iterators")              ;востребованных
        arg1 (gensym "arg1")                                ;значений
        arg2 (gensym "arg2")]
    `(let [~finished (atom false)                           ;Указатель на завершение работы
           ~bindings (atom '~user-bindings)                 ;Сочетание символа и его значения
           ~return-value (atom nil)
           ~'break (delay (throw                            ; "Команда" завершения
                            (ex-info nil {:type :break})))  ; работы цикла
           ~'continue (delay
                        (throw                              ; "Команда" перехода на
                          (ex-info nil {:type :continue}))) ; следующую итерацию

           ~post-exp (atom nil)                             ; Выражение, преобразующее итератор

           ~all-iterators (atom                             ; Все прошедшие
                            (conj [] (second @~bindings)))  ; итераторы

           ~arg1 '~(nth user-bindings 2)                    ;см. for-loop
           ~arg2 '~(nth user-bindings 3)]                   ;см. for-loop

       (loop [reducing-body# '~body]
         (if (= (str (first reducing-body#)) "my-recur")    ;получаем post-expression
           (reset! ~post-exp (last reducing-body#))
           (recur (last reducing-body#))))

       (try
         (while (not @~finished)
           (reset! ~finished true)
           (try
             (reset! ~return-value
                     (apply
                       (fn ~(vec (conj '() (first user-bindings)))
                         ~@(rw/walk-exprs                   ; выполнение body,
                             recur-statement?               ; преобразование итератора
                             (partial recur-expression finished bindings)
                             body))
                       (conj '() (second @~bindings))))

             (reset! ~all-iterators                         ; добавление нынешнего
                     (conj @~all-iterators                  ; итератора
                           (second @~bindings)))
             (catch Exception e#                            ; здесь ловится exception
               (reset! ~bindings [(first @~bindings)        ; и проверяется его тип
                                  (@(resolve                ; если это break - то кидаем
                                      (first @~post-exp))   ; еще один exception в "верхний"
                                    (second @~bindings))])  ; блок try/catch, иначе -
               (when-not (= :continue (:type (ex-data e#))) ; переходим к следующей итерации
                 (throw e#))
               (reset! ~finished false)
               (reset! ~all-iterators
                       (assoc @~all-iterators
                         (dec (count @~all-iterators))
                         (@(resolve (first @~post-exp))
                           (last @~all-iterators)))))))
         (catch Exception e#                                ; "верхний" блок try/catch,
           (when-not (= :break (:type (ex-data e#)))        ; предназначенный для того,
             (reset! ~all-iterators                         ; чтобы "ловить" команду "break"
                     (conj @~all-iterators 0)))))

       (if (= (last @~all-iterators) (last (drop-last @~all-iterators)))
         (reset! ~all-iterators (vec (drop-last (drop-last @~all-iterators))))
         (reset! ~all-iterators (vec (drop-last @~all-iterators))))

       ;(println "all iteration elements" @~all-iterators)
       (if (= nil ~arg1)                                    ; применение нужной функции
         @~all-iterators                                    ; к all-iterators в зависимости
         (if (= (str ~arg1) "reduce")                       ; от переданных аргументов
           (reduce (resolve ~arg2) @~all-iterators)         ; arg1 и arg2
           (if (= ~arg2 nil)
             (transduce ~(nth user-bindings 2) conj @~all-iterators)
             (transduce ~(nth user-bindings 2) (resolve ~arg2)
                        @~all-iterators)))))))

(defmacro for-loop
  ;;;;; arg1 и arg2 передаются в функцию, чтобы понять, какое действие совершить
  ;;;;; над итоговым значением. Изначально возвращается список пройденных
  ;;;;; значений итератора(для получения требуется отправить nil nil). Чтобы применить
  ;;;;; map отправляется (map *action*) + nil или */-+. Фильтрация - идентично
  ;;;;; (filter *predicate*) + nil или */-+. Для свертки - reduce + +/*-.

  ;;;Реализация  цикла с глобальной константой
  ([symbol# check-exp post-exp
    arg1 arg2
    body]
   (let [body-tmp (concat body (list (list 'my-recur post-exp)))
         init-value @(resolve symbol#)]
     `(my-loop [~symbol# ~init-value
                ~arg1 ~arg2]
               (if ~check-exp
                 (do
                   ~@body-tmp)))))

  ;;;Реализация с (range)
  ([symbol range
    arg1 arg2
    body]
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
       `(my-loop [~symbol ~initial-value
                  ~arg1 ~arg2]
                 (if ~check-expression
                   (do
                     ~@body-tmp))))))

  ;;;Обычный (for j := 7; j <= 9; j++)
  ([symbol init-value check-exp post-exp
    arg1 arg2
    body]
   (let [body-tmp (concat body (list (list 'my-recur post-exp)))]
     `(my-loop [~symbol ~init-value
                ~arg1 ~arg2]
               (if ~check-exp
                 (do
                   ~@body-tmp))))))

(def i 1)
(println "macro output is : "
         (macroexpand
           (for-loop i (< i 20) (inc i)
                     reduce +
                     ((if (= i 3) @continue)
                       (if (= i 8) @break)
                       (println i)))))


(println "macro output is : "
         (macroexpand
           (for-loop i 10 (> i 2) (dec i)
                     (map inc) +
                     ((if (= i 5)
                        @continue)
                       (println i)))))

(println "macro output is : "
         (macroexpand
           (for-loop i (range 2 10)
                     (map inc) nil
                     ((if (= i 3) @continue)
                       (if (= i 7) @break)
                       (println i)))))
