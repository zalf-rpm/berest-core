(ns de.zalf.berest.core.util
  (:require [clj-time.core :as ctc]
            [clj-time.coerce :as ctcoe]
            [clojure.string :as str]
            [de.zalf.berest.core.helper :as h]))

(defn round [value & {:keys [digits] :or {digits 0}}]
  (let [factor (Math/pow 10 digits)]
    (-> value
        (* factor)
        Math/round
        (/ factor))))

(defn >=2digits [number]
  (if (< number 10) (str "0" number) (str number)))

(defn date-to-doy
  "get day of year (doy) either from a date with its constituents
  or from a java.util.Date"
  ([day month & [year]]
   (.. (ctc/date-time (or year 2010) month day) getDayOfYear))
  ([date]
   (.getDayOfYear (ctcoe/from-date date))))

(defn doy-to-date
  [doy & [year]]
  (ctc/plus (ctc/date-time (or year 2010) 1 1) (ctc/days (dec doy))))

(def sum (partial reduce + 0))

(defn scalar-op [op scalar vector]
  (map #(op scalar %) vector))

(def s-add (partial scalar-op +))

(def s-mult (partial scalar-op *))

(defn dot-op [op vec1 vec2]
  (map #(op %1 %2) vec1 vec2))

(def dot-add (partial dot-op +))

(def dot-mult (partial dot-op *))


(defn split-stt-code
  [stt-code]
  (if (or (> stt-code 693) (< stt-code 100))
    []
    (let [_1 (int (/ (double stt-code) 100.0))
          _2 (int (/ (mod (double stt-code) 100)
                     10.0))
          _3 (- stt-code (* _1 100) (* _2 10))]
      (if (> _3 3)
        []
        [_1 _2 _3]))))

(defn code->stt*
  [stt-code]
  (if-let [[_1 _2 _3] (not-empty (split-stt-code stt-code))]
    [(case _1
       1 "D"
       2 "M"
       3 "Al"
       4 "Lö"
       5 "V"
       6 "K")
     _2
     (case _3
       1 "a"
       2 "b"
       3 "c"
       4 "d")]))

(def code->stt (h/rcomp code->stt* str/join))

(defn code->stts
  [stt-code]
  (->> stt-code
       code->stt*
       (reductions #(conj %1 %2) [],,,)
       rest
       (mapv str/join,,,)))

(defn stt->code
  [stt]
  (let [[_ _1 _2 _3] (re-matches #"([^\d]{1,2})(\d)(\w)" (str/lower-case stt))]
    (+ (case _1
         "d" 100
         "m" 200
         "al" 300
         "lö" 400
         "v" 500
         "k" 600)
       (* (Integer/parseInt _2) 10)
       (case _3
         "a" 1
         "b" 2
         "c" 3
         "d" 4))))


(defn parse-german-double
  [text]
  (double (.. java.text.NumberFormat (getInstance java.util.Locale/GERMAN) (parse text))))