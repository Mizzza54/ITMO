(defn operation [op] (fn [& arg] (apply mapv op arg)))
(defn minor [a b i j] (- (* (nth a i) (nth b j)) (* (nth a j) (nth b i))))

(def v+ (operation +))
(def v- (operation -))
(def v* (operation *))
(def vd (operation /))
(defn v*s [a b] (mapv (fn [element] (* b element)) a))
(defn scalar [a b] (reduce + (v* a b)))
(defn vect [a b] [(minor a b 1 2) (minor a b 2 0) (minor a b 0 1)])

(def m+ (operation v+))
(def m- (operation v-))
(def m* (operation v*))
(def md (operation vd))
(defn m*s [a b] (mapv (fn [line] (v*s line b)) a))

(defn m*v [a b] (mapv (fn [line] (scalar line b)) a))

(defn transpose [m] (apply mapv vector m))

(defn m*m [a b] (mapv (partial m*v (transpose b)) a))

(def c+ (operation m+))
(def c- (operation m-))
(def c* (operation m*))
(def cd (operation md))