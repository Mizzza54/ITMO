(defn proto-get [obj key]
  (cond
    (contains? obj key) (obj key)
    (contains? obj :prototype) (proto-get (obj :prototype) key)
    :else nil))

(defn proto-call [this key & args]
  (apply (proto-get this key) this args))

(defn field [key]
  (fn [this] (proto-get this key)))

(defn method [key]
  (fn [this & args] (apply proto-call this key args)))

(def toString (method :toString))
(def toStringSuffix (method :toStringSuffix))
(def evaluate (method :evaluate))
(def diff (method :diff))
(def operands (field :operands))


(defn Constant [number]
  {
    :toString       (fn [this] (format "%.1f" number))
    :toStringSuffix toString
    :evaluate       (fn [this _] number)
    :diff           (fn [_ _] (Constant 0))
    }
  )

(def ZERO (Constant 0.0))
(def ONE (Constant 1.0))


(defn Variable [name]
  (let [nameReal (str (first (clojure.string/lower-case name)))]
  {
    :toString       (fn [this] name)
    :toStringSuffix toString
    :evaluate       (fn [this id] (id nameReal))
    :diff           (fn [this id] (if (= nameReal id) ONE ZERO))
    }
  ))

(defn createOperation [sign action howToDiff]
  (fn [& operands]
    {
      :evaluate       (fn [this vars] (apply action (mapv (fn [operand] (evaluate operand vars)) operands)))
      :toString       (fn [this] (str "(" sign " " (clojure.string/join " " (mapv toString operands))")"))
      :toStringSuffix (fn [this] (str "(" (clojure.string/join " " (mapv toStringSuffix operands)) " " sign ")"))
      :operands       (vec operands)
      :diff           (fn [this var]
                        (cond
                          (= 2 (count operands)) (howToDiff (first operands) (last operands) (diff (first operands) var) (diff (last operands) var))
                          (= 1 (count operands)) (howToDiff (first operands) (diff (first operands) var))))
      }
    )
  )

(def Add
  (createOperation '+ + (fn [_ _ da db] (Add da db))))

(def Subtract
  (createOperation '- - (fn [_ _ da db] (Subtract da db))))

(def Multiply
  (createOperation '* * (fn [a b da db] (Add (Multiply da b) (Multiply db a)))))
(def Divide
  (createOperation '/ (fn [x y] (/ x (double y)))
                   (fn [a b da db] (Divide (Subtract (Multiply b da)
                                                     (Multiply a db))
                                           (Multiply b b)))))
(def Negate
  (createOperation 'negate - (fn [_ da] (Subtract da))))
(def Exp
  (createOperation 'exp (fn [x] (Math/exp x)) (fn [a da] (Multiply da (Exp a)))))

(def Ln
  (createOperation 'ln (fn [x] (Math/log (Math/abs x))) (fn [a da] (Multiply da (Divide ONE a)))
                   ))

(def objectOperations
  {
    '+      Add
    '-      Subtract
    '*      Multiply
    '/      Divide
    'negate Negate
    'exp    Exp
    'ln     Ln
    })

(defn parseObject [expression]
  (cond
    (string? expression) (parseObject (read-string expression))
    (number? expression) (Constant expression)
    (symbol? expression) (Variable (str expression))
    (seq? expression) (apply (objectOperations (first expression)) (mapv parseObject (rest expression)))))

(defn -return [value tail] {:value value :tail tail})
(def -valid? boolean)
(def -value :value)
(def -tail :tail)
(defn _show [result] (if (-valid? result) (str "-> " (pr-str (-value result)) " | " (pr-str (apply str (-tail result)))) "!"))
(defn tabulate [parser inputs] (run! (fn [input] (printf "    %-10s %s\n" (pr-str input) (_show (parser input)))) inputs))
(defn _empty [value] (partial -return value))
(defn _char [p] (fn [[c & cs]] (if (and c (p c)) (-return c cs))))
(defn _map [f result] (if (-valid? result) (-return (f (-value result)) (-tail result))))
(defn _combine [f a b] (fn [str] (let [ar ((force a) str)] (if (-valid? ar) (_map (partial f (-value ar)) ((force b) (-tail ar)))))))
(defn _either [a b] (fn [str] (let [ar ((force a) str)] (if (-valid? ar) ar ((force b) str)))))
(defn _parser [p] (fn [input] (-value ((_combine (fn [v _] v) p (_char #{\u0000})) (str input \u0000)))))

(defn +char [chars] (_char (set chars)))
(defn +map [f parser] (comp (partial _map f) parser))
(def +parser _parser)
(def +ignore (partial +map (constantly 'ignore)))
(defn iconj [coll value] (if (= value 'ignore) coll (conj coll value)))
(defn +seq [& ps] (reduce (partial _combine iconj) (_empty []) ps))
(defn +seqf [f & ps] (+map (partial apply f) (apply +seq ps)))
(defn +seqn [n & ps] (apply +seqf (fn [& vs] (nth vs n)) ps))
(defn +or [p & ps] (reduce _either p ps))
(defn +opt [p] (+or p (_empty nil)))
(defn +star [p] (letfn [(rec [] (+or (+seqf cons p (delay (rec))) (_empty ())))] (rec)))
(defn +plus [p] (+seqf cons p (+star p)))
(defn +str [p] (+map (partial apply str) p))

(def *all-chars (mapv char (range 0 1024)))
(def *digit (+char "0123456789.-"))
(def *number (+map read-string (+str (+plus *digit))))
(def *space (+char " \t\n\r"))
(def *spaces (apply str (filter #(Character/isWhitespace %) *all-chars)))
(def *ws (+ignore (+star *space)))
(defn *seq [begin p end] (+seqn 1 (+char begin) (+opt (+seqf cons *ws p (+star (+seqn 0 *ws p)))) *ws (+char end)))




(def *operations (+char "+-*/"))

(def *negate (+seqf list (+char "n") (+char "e") (+char "g") (+char "a") (+char "t") (+char "e")))

(def *expression (+map symbol (+str (+or (+seqf cons (+char "XYZxyz") (+star (+char "XYZxyz"))) (+seqf list *operations)
                                                                                                  *negate
  ))))

(declare *value)
(defn *list [] (+seqf (fn [tmp] (cons (last tmp) (drop-last tmp))) (*seq "(" (delay (*value)) ")")))
(defn *value [] (+or *number *expression (*list)))
(defn parseObjectSuffix [expr] (parseObject ((+parser (+seqn 0 *ws (*value) *ws)) expr)))