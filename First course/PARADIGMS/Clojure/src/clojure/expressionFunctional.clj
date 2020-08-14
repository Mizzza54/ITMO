(defn variable [name] (fn [vars] (get vars name)))
(defn constant [value] (fn [trash] value))

(defn operation [f]
  (fn [& arguments]
    (fn [args]
      (apply f (mapv (fn [g] (g args)) arguments)))))


(def add (operation +))
(def subtract (operation -))
(def multiply (operation *))
(def divide (operation (fn [x y] (/ (double x) (double y)))))
(def negate (operation -))
(def min (operation clojure.core/min))
(def max (operation clojure.core/max))
(def exp (operation (fn [x] (Math/exp x))))
(def ln (operation (fn [x] (Math/log (Math/abs x)))))


(def operations
  {'+ add,
   '- subtract,
   'negate negate,
   '* multiply,
   '/ divide,
   'min min,
   'max max,
   'exp exp,
   'ln ln
   })

(defn parseFunction [expression]
  (cond
    (string? expression) (parseFunction (read-string expression))
    (number? expression) (constant expression)
    (symbol? expression) (variable (str expression))
    (seq? expression) (apply (get operations (first expression)) (mapv parseFunction (rest expression)))))