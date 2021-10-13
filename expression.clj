(defn variable [value] #(% value))

(def constant constantly)

(defn create-operation [op]
    (fn [& args]
        (fn [vars]
            (apply op (mapv #(% vars) args)))))

(defn divide-op [& args]
    (if (== 1 (count args))
    (/ 1.0 (double (first args)))
    (reduce #(/ (double %1) (double %2)) (first args) (rest args))))

(def add (create-operation +))
(def subtract (create-operation -))
(def multiply (create-operation *))
(def divide (create-operation divide-op))
(def negate subtract)

(defn mean-op [& args] (divide-op (apply + args) (count args)))
(defn varn-op [& args]
    (let [mean-args (apply mean-op args)
          mean-sqr-args (apply mean-op (mapv #(* % %) args))]
         (- mean-sqr-args (* mean-args mean-args))))

(def mean (create-operation mean-op))
(def varn (create-operation varn-op))

(def func-names
    {'+ add
     '- subtract
     '* multiply
     '/ divide
     'negate negate
     'mean mean
     'varn varn})

(defn abstract-parse [dict const-impl var-impl]
    #(letfn [(parse-item [item]
             (cond
             (list? item)
                 (apply ((first item) dict) (mapv parse-item (rest item)))
             (number? item) (const-impl item)
             :else (var-impl (str item))))]
         (parse-item (read-string %))))

(def parseFunction (abstract-parse func-names constant variable))

(load-file "proto.clj")

(def evaluate (method :evaluate))
(def toString (method :toString))
(def diff (method :diff))

(defn expression-prototype [evaluate_ to-string_ diff_]
    {:evaluate evaluate_
	    :toString to-string_
	    :diff diff_})
	
(declare zero-constant)
(def Constant
    (constructor
		      (fn [this value_] 
		          (assoc this 
		              :value value_))
		      (expression-prototype
		          (fn [this vars] ((field :value) this))
		          #(str ((field :value) %))
			         (fn [this diff-name] zero-constant))))

(def zero-constant (Constant 0))
(def one-constant (Constant 1))
(def two-constant (Constant 2))

(def Variable
    (constructor
        (fn [this var-name_]
		          (assoc this 
		              :var-name var-name_))
		      (expression-prototype
			         (fn [this vars] (vars ((field :var-name) this)))
			         (field :var-name)
			         (fn [this diff-name] 
			             (if (= diff-name ((field :var-name) this))
			             one-constant
			             zero-constant)))))

(def operation-prototype
    (expression-prototype
		      (fn [this vars] 
		          (apply 
		    	         ((field :operation) this)
		              (mapv #(evaluate % vars) ((field :args) this))))
		      (fn [this] 
		          (str "(" ((field :operation-sign) this) " " (clojure.string/join " " (mapv toString ((field :args) this))) ")"))
		      (fn [this diff-name] 
		          (((field :diff-impl) this) 
		    	         ((field :args) this) 
		              (mapv #(diff % diff-name) ((field :args) this))))))

(defn operation-factory [operation_ operation-sign_ diff-impl_]
    (constructor
		      (fn [this & args] 
		          (assoc this
		              :args (vec args)))
		  (assoc operation-prototype
	       :operation operation_
	       :operation-sign operation-sign_
	       :diff-impl diff-impl_)))
	    
(def Negate (operation-factory - "negate" (fn [_ diff-args-vec] (Negate (first diff-args-vec)))))

(def Add (operation-factory + "+" (fn [_ diff-args-vec] (apply Add diff-args-vec))))

(def Subtract (operation-factory - "-" (fn [_ diff-args-vec] (apply Subtract diff-args-vec))))

(declare Multiply)
(defn multiply-diff-impl [args-vec diff-args-vec]
    (second 
		      (reduce 
	    	      (fn [[fst dfst] [scnd dscnd]]
	        	      [(Multiply fst scnd)
	                (Add (Multiply fst dscnd) (Multiply scnd dfst))])
	           (mapv vector args-vec diff-args-vec))))

(def Multiply (operation-factory * "*" multiply-diff-impl))

(def Power
    (operation-factory
        (fn [base exp] (Math/pow base exp))
        "power"
        (fn [args-vec diff-args-vec] 
            (let [base (first args-vec)
                  exp (nth 1 args-vec)
                  diff-base (first diff-args-vec)]
                  (Multiply
                      exp
                      (Power
                          base
                          (Subtract exp one-constant))
                      diff-base)))))

(def Divide 
    (operation-factory 
        divide-op 
        "/" 
        (fn [args-vec diff-args-vec]
            (let [first-args (first args-vec)
	                 rest-args (vec (rest args-vec))
	                 first-diff-args (first diff-args-vec)
	                 rest-diff-args (vec (rest diff-args-vec))]
	                (if (empty? rest-args)
	                (Negate 
	    	               (Divide 
	    		                  first-diff-args 
	    		                  (Multiply first-args first-args)))
	                    (let [denom (apply Multiply rest-args)
	                          diff-denom (multiply-diff-impl rest-args rest-diff-args)]
	                         (Divide
	           		                (Subtract
	           			                   (Multiply first-diff-args denom)
	           			                   (Multiply first-args diff-denom))
	           		            (Power denom two-constant))))))))

(def Absolute 
    (operation-factory 
        #(Math/abs %)
        "absolute"
        (fn [args-vec diff-args-vec]
            (Multiply
                (Divide (first args-vec) (Absolute (first args-vec)))
                (first diff-args-vec)))))

(def ArithMean 
    (operation-factory
        (fn [& args] (divide-op (apply + args) (count args))) 
        "arith-mean" 
        (fn [args-vec diff-args-vec] (Divide 
            (apply Add diff-args-vec) 
            (Constant (count diff-args-vec))))))

(def GeomMean 
    (operation-factory 
        (fn [& args] 
            (Math/pow 
                (Math/abs (apply * args)) 
                (divide-op (count args))))
        "geom-mean" 
        (fn [args-vec diff-args-vec]
            (let [n (count args-vec)
                  mult-args (apply Multiply args-vec)
                  sign-product (Divide mult-args (Absolute mult-args))]
                 (Multiply
                     sign-product
                     (Divide 
                         (multiply-diff-impl args-vec diff-args-vec)
                         (Multiply 
                             (Constant n) 
                             (Power 
                                 (Absolute mult-args) 
                                 (Divide 
                                     (Subtract 
                                         (Constant n) one-constant) 
                                         (Constant n))))))))))

(def HarmMean 
    (operation-factory 
        (fn [& args] 
            (divide-op 
                (count args) 
                (apply + (mapv #(divide-op 1 %) args)))) 
        "harm-mean" 
        (fn [args-vec diff-args-vec]
            (Divide
                (Multiply
                    (Constant (count args-vec))
                    (apply Add (mapv #(Divide %2 (Multiply %1 %1)) args-vec diff-args-vec)))
                (Power (apply Add (mapv #(Divide one-constant %) args-vec)) two-constant)))))

(def obj-names
    {'+ Add
     '- Subtract
     '* Multiply
     '/ Divide
     'negate Negate
     'absolute Absolute
     'power Power
     'arith-mean ArithMean
     'geom-mean GeomMean
     'harm-mean HarmMean})

(def parseObject (abstract-parse obj-names Constant Variable))