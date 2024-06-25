(ns homework3)
(defn custom-eval [expr env]
  (cond

    (list? expr)
    (let [[op & args] expr]
      (cond
        (= op '+) (apply + (map #(custom-eval % env) args))
        (= op '-) (apply - (map #(custom-eval % env) args))
        (= op '*) (apply * (map #(custom-eval % env) args))
        (= op '/) (apply / (map #(custom-eval % env) args))
        (= op 'defn)
        (let [[name params body] args
              fn (fn [& fn-args]
                   (let [local-env (merge env (zipmap params fn-args))]
                     (custom-eval body local-env)))]
          (assoc env name fn))

        :else (let [f (custom-eval op env)]
                (if (fn? f)
                  (apply f (map #(custom-eval % env) args))
                  (throw (Exception. (str "Undefined function: " op)))))))

    (symbol? expr)
    (if (contains? env expr)
      (get env expr)
      (throw (Exception. (str "Undefined variable: " expr))))

    (number? expr) expr

    :else (throw (Exception. (str "Unsupported expression: " expr)))))

(def initial-env {'x 10 'y 20 '+ + '- - '* * '/ /})

(def updated-env
  (custom-eval '(defn sum [a b] (+ a b)) initial-env))

(println "Test Function Call: " (= (custom-eval '(sum x y) updated-env) 30)) ;; => true
(println "Test Function Call with Literals: " (= (custom-eval '(sum 15 25) updated-env) 40)) ;; => true

(println "Error Handling: "
         (try
           (custom-eval '(sum x z) {:x 1 :y 2})
           (catch Exception e (.getMessage e)))) 