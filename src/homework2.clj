(ns homework2)
(defn compose
  [& fns]
  (fn [& args]
    (let [fns (reverse fns)]
      (reduce (fn [val f]
                (f val))
              (apply (first fns) args)
              (rest fns)))))
    (println(= [3 2 1] ((compose rest reverse) [1 2 3 4])))

    (println(= 5 ((compose (partial + 3) second) [1 2 3 4])))

    (println(= true ((compose zero? #(mod % 8) +) 3 5 7 9)))

    (println(= "HELLO" ((compose #(.toUpperCase %) #(apply str %) take) 5 "hello world")))


(defn merge-with-fn
  [f & maps]
  (let [result (volatile! {})]
    (doseq [m maps]
      (vswap! result
              (fn [acc]
                (reduce (fn [acc [k v]]
                          (assoc acc k
                                 (let [existing (get acc k)]
                                   (if (nil? existing)
                                     v
                                     (f existing v)))))
                        acc m))))
    @result))

(println
  (= (merge-with-fn * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
     {:a 4, :b 6, :c 20}))

(println
  (= (merge-with-fn - {1 10, 2 20} {1 3, 2 10, 3 15})
     {1 7, 2 10, 3 15}))

(println
  (= (merge-with-fn concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})
     {:a [3 4 5], :b [6 7], :c [8 9]}))
(defn juxt
  [& fns]
  (fn
    [& args]
    (map (fn [f] (apply f args)) fns)))
(println    (= [21 6 1] ((juxt + max min) 2 3 5 1 6 4)))

    (println(= ["HELLO" 5] ((juxt #(.toUpperCase %) count) "hello")))

    (println(= [2 6 4] ((juxt :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10})))

(defn uncurry [f]
  (let [f-meta (meta f)
        arity (:arglists f-meta)]
    (fn [& args]
      (if (= (count args) (count arity))
        (apply f args)
        ((apply partial f (take (count arity) args)) (drop (count arity) args))))))
[(= 10 ((uncurry (fn [a]
                   (fn [b]
                     (fn [c]
                       (fn [d]
                         (+ a b c d))))))
        1 2 3 4))
 (= 24 ((uncurry (fn [a]
                   (fn [b]
                     (fn [c]
                       (fn [d]
                         (* a b c d))))))
        1 2 3 4))
 (= 25 ((uncurry (fn [a]
                   (fn [b]
                     (* a b))))
        5 5))]
