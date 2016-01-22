(ns de.zalf.berest.core.helper)

(defn thrush [& args]
  (reduce #(%2 %1) args))

(defn rcomp
  "reverse compose function, composes arguments from left to right
  (pipeline order)"
  [& args]
  (apply comp (reverse args)))

#_(def |-> "symbol/shortcut for rcomp (reverse compose) function"
  rcomp)

#_(def |<- "symbol/shortcut for comp (compose) function"
  comp)

#_(def --< "symbol/shortcut for juxt function"
  juxt)

#_(def |* "symbol/shortcut for partial function"
  partial)

(defn partial-kw
  "partial function which works with keyword (optional) arguments"
  [f & kw-args]
  (fn [& args]
    (apply f (concat args kw-args))))

#_(def |*kw "symbol/shortcut for partial-kw function"
  partial-kw)

(defn ajuxt
  "juxts result directly applied to last argument, thus
  directly useable as a function with variable arguments
  (at least be two)"
  [& rest]
  ((apply juxt (butlast rest)) (last rest)))

#_(def --<* "symbol/shortcut for juxt*"
  juxt*)

(defn args-21->12
  "swap the two arguments before applying f to them"
  [f arg2 arg1]
  (f arg1 arg2))

(def swap
  "swap the two arguments before applying them to f"
  args-21->12)

(defn args-231->123
  "rotate arguments in the order described by the function's name"
  [f arg2 arg3 arg1]
  (f arg1 arg2 arg3))
