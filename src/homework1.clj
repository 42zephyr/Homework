(ns homework1)
(def result (atom {:correct 0 :incorrect 0}))
(defmacro =check [left right]
  `(let [left# ~left
         right# ~right]
     (if (= right# left#)
       (swap! result update :correct inc)
       (do
         (println "missmatch:" ~(str (first left)) left# right#)
         (swap! result update :incorrect inc)))
     left#))

;; reduce examples
(defn my-reduce [f init coll]
  (loop [result init                   
         restt coll]         
    (if (empty? restt)     
      result                        
      (recur (f result (first restt)) (rest restt))
      ))
) 

(=check (my-reduce + 0 [1 2 3 4]) 10)
(=check (my-reduce str "" ["a" "b" "c"]) "abc")
(=check (my-reduce + 0 (range 10000)) 49995000)

;; filter examples
(defn my-filter [pred coll]
    (let [result []]
        (if (empty? coll)
            result
            (let [head (last coll) tail (butlast coll)]
                (if (pred head)
                    (conj (my-filter pred tail) head)
                    (my-filter pred tail)
                ))
            )))

(=check (my-filter even? [1 2 3 4 5 6]) [2 4 6])
(=check (my-filter #(> (count %) 3) ["hi" "hello" "hey" "greetings"]) ["hello" "greetings"])
(=check (my-filter #(and (even? %) (> % 10)) [12 2 13 14 3]) [12 14])

;; concat examples
(defn my-concat [coll1 coll2]
      (if (empty? coll1)
    coll2
    (recur (butlast coll1) (cons (last coll1) coll2))
    )
)

(=check (my-concat [1 2] [3 4]) [1 2 3 4])
(=check (count (my-concat (range 5000) (range 5000 10000))) 10000)

;; nth examples
(defn my-nth [coll index]
    (if (= 0 index)
        (first coll)
        (my-nth (rest coll) (dec index))
        )
    )
(=check (my-nth [10 20 30 40] 2) 30)
;; (=check (my-nth [1 2 3 4] 10) nil) ; Assuming nil for out of bounds
(=check (my-nth [1 2 3 4] 3) 4)

;; max/min examples
(defn my-max [coll]
  (if (empty? coll)
    nil
    (if (= (count coll) 1)
      (first coll)
      (if (> (first coll) (my-max (rest coll)))
        (first coll)
        (my-max (rest coll))))
    )
)

(defn my-min [coll]
  (if (empty? coll)
    nil
    (if (= (count coll) 1)
      (first coll)
      (if (< (first coll) (my-min (rest coll)))
        (first coll)
        (my-min (rest coll))))
    )
)
(=check (my-max [5 3 9 1]) 9)
(=check (my-min [5 3 9 1]) 1)
(=check (my-max [-5 -3 -9 -1]) -1)
(=check (my-min [-5 -3 -9 -1]) -9)
(=check (my-max []) nil)
(=check (my-min []) nil)

;; count examples
(defn my-count [coll]
    (println (empty? coll))
    (if (empty? coll)
        0
        (+ 1 (my-count (rest coll) ) )
        )
    )
(=check (my-count [1 2 3 4 5]) 5)
(=check (my-count [[1 2] [3 4] [5]]) 3)
(=check (my-count []) 0)

;; take examples
(defn my-take [n coll]
    (if (= n 0)
        []
        (cons (first coll) (my-take (dec n) (rest coll)))
        )   
    )
(=check (my-take 3 [5 4 3 2 1]) [5 4 3])

;; merge examples
(defn my-merge [map1 map2]
  (let [merged-map (atom {})]
    (doseq [[k v] map1]
      (swap! merged-map assoc k v))
    (doseq [[k v] map2]
      (swap! merged-map assoc k v))
    @merged-map))
(=check (my-merge {:a 1 :b 2} {:b 3 :c 4}) {:a 1 :b 3 :c 4})
(=check (my-merge {:foo "bar"} {:foo "baz", :hello "world"}) {:foo "baz", :hello "world"})
(=check (my-merge {} {:a 1}) {:a 1})

;; group-by examples
(defn my-group-by [f coll]
  (reduce
    (fn [acc x]
      (let [key (f x)]
        (assoc acc key (conj (get acc key []) x))))
    {}
    coll))
(=check (my-group-by :type [{:type :a :value 1} {:type :b :value 2} {:type :a :value 3}])
        {:a [{:type :a :value 1} {:type :a :value 3}], :b [{:type :b :value 2}]})
(=check (my-group-by even? [1 2 3 4 5 6]) {true [2 4 6], false [1 3 5]})
(=check (my-group-by count ["one" "two" "three" "four"]) {3 ["one" "two"], 5 ["three"], 4 ["four"]})

;; keys examples
(defn my-keys [map]
   (let [keyss (atom [])]
    (doseq [[k val] map]
      (swap! keyss conj k))
    @keyss))
(=check (my-keys {:a 1 :b 2 :c 3}) [:a :b :c])
(=check (my-keys {:foo "bar" :baz "qux"}) [:foo :baz])
(=check (my-keys {}) [])

;; vals examples
(defn my-vals [map]
   (let [vals (atom [])]
    (doseq [[_ val] map]
      (swap! vals conj val))
    @vals))
(=check (my-vals {:a 1 :b 2 :c 3}) [1 2 3])
(=check (my-vals {:foo "bar" :baz "qux"}) ["bar" "qux"])
(=check (my-vals {}) [])

;; select-keys examples
(defn my-select-keys [map keys]
  (let [result (atom {})]
    (doseq [[k val] map]
      (if (contains? (set keys) k)
        (swap! result assoc k val)))
    @result)
)
(=check (my-select-keys {:a 1 :b 2 :c 3} [:a :c]) {:a 1 :c 3})
(=check (my-select-keys {:name "Alice" :age 30 :gender "Female"} [:name :age]) {:name "Alice", :age 30})
(=check (my-select-keys {:foo "bar" :hello "world"} [:foo]) {:foo "bar"})

(println "Test results:" @result)
