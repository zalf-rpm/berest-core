(ns de.zalf.berest.core.core
  (:require [clojure.math.numeric-tower :as nt]
            [clojure.java.io :as cjio]
            clojure.set
            [clojure.string :as str]
            [clojure.pprint :as pp]
            [clj-time.core :as ctc]
            [clj-time.format :as ctf]
            [clj-time.coerce :as ctcoe]
            [de.zalf.berest.core.datomic :as db]
            [de.zalf.berest.core.util :as bu]
            [clojure-csv.core :as csv]
            [datomic.api :as d]
            [de.zalf.berest.core.climate.algo :as algo]
            [de.zalf.berest.core.helper :as bh :refer [rcomp ajuxt]]
            [de.zalf.berest.core.data :as data]
            [clojure.algo.generic.functor :as cagf :refer [fmap]]
            [taoensso.timbre :as timbre :refer [trace debug info warn error fatal spy]]

            #_[clojure.tools.macro :as ctm]
            #_[clojure.algo.generic.arithmetic :as caga]
            #_[incanter.core :as ic]))

(timbre/set-level! :info)
(timbre/set-config! [:timestamp-pattern] ""#_"yyyy-MMM-dd HH:mm:ss ZZ")
(timbre/set-config! [:timestamp-locale] (java.util.Locale/GERMAN))
(timbre/set-config! [:prefix-fn] (fn [{:keys [level timestamp hostname ns]}]
                                   (str #_timestamp #_" " #_hostname #_" " (-> level name str/upper-case)
                                        " [" ns "]")))

;(frinj-init!)

(def ^{:dynamic true :berest/unit :cm} *layer-sizes* (flatten [5 5 (repeat 19 10)]))
#_(def ^{:dynamic true :berest/unit :cm} *layer-sizes* [10 20 30 40 50])
(defn ^{:berest/unit :cm} layer-depths [] (reductions + *layer-sizes*))
(defn no-of-soil-layers [] (count *layer-sizes*))
(defn ^{:berest/unit :cm} max-soil-depth [] (last (layer-depths)))

(defn pFK->mm7x
  "fc [mm/x] -> [mm/x]"
  [fc percent-value]
  (* 1/100 percent-value fc))

(defn pNFK->mm7x
  "fc, pwp [mm/x] -> [mm/x]"
  [fc pwp percent-value]
  (+ pwp (* 1/100 percent-value (- fc pwp))))

(defn mm7x->pNFK
  "fc, pwp, mm-value [mm/x] -> [% NFK]"
  [fc pwp sm-value]
  (/ (- sm-value pwp)
     (* (- fc pwp) 100)))

(defn volp->mm7dm
  "value [volp] -> [mm/dm] -> "
  [value]
  value)

(defn mm7dm->volp
  "value [mm/dm] -> [volp]"
  [value]
  value)

(defn mm7dm->mm7cm
  "value [mm/dm] -> [mm/cm]"
  [value]
  (/ value
     10))

(defn mm7cm->mm7dm
  "value [mm/cm] -> [mm/dm]"
  [value]
  (* value 10))

(def fout (ref []))
(def out (ref ""))

(defn append-out
  [out append-func value]
  (dosync
   (alter out append-func value)))

(defn nFC
  [fc pwp]
  (- fc pwp))

(defn sorted-unzip-map
  "split a map into it's keys and values by sorted keys, return [[keys][values]]"
  [map]
  ((juxt keys vals) (into (sorted-map) map)))

(defn expand-layers
  "(expand-fn value expanded-depth)
  (last-values-fn expanded-values last-value) ... returns function which takes like expand-fn
  [value depth] but knows how to treat 'values' below last depths until max-depth,
  the default being to simply repeat the last-value from 'values' parameter"
  [max-depth [depths values] & {:keys [expand-fn last-values-fn]
                                :or {expand-fn (fn [value depth] value)
                                     last-values-fn (fn [expanded-values last-value]
                                                      (fn [value depth] last-value))}}]
  {:pre [(= (count depths) (count values))]}
  (let [;create pairs of upper and lower bounds
        bounds (partition 2 1 (into (sorted-set) (flatten [0 (take-while #(<= % max-depth) depths) max-depth])))

        ;create sizes out of the list of bounds
        depths* (map (fn [[l r]] (- r l)) bounds)

        ;associate the according values to the depths just created, but only until the depth available in depths-list
        expanded-values (mapcat #(repeat %1 (expand-fn %2 %1)) depths* values)]

    ;now add potentially values filling up to max-depths, the function last-values-fn
    ;can examine the stuff done so far and get the last value
    #_(println "expanded-values: " (count expanded-values) " max-depth: " max-depth " last depths*: " (last depths*)
             " last expanded-values: " (last expanded-values))
    (if (< (count expanded-values) max-depth)
      (concat expanded-values
              (repeat (last depths*)
                      ((last-values-fn expanded-values (last values)) (last values) (last depths*))))
      expanded-values)))

(defn aggregate-layers
  "aggregate layers according to given layer-sizes and reduce function reduce-fn"
  [reduce-fn layer-sizes layer-data]
  (->> (loop [res []
              sizes layer-sizes
              data layer-data]
         (if (not (seq sizes))
           (if (seq data)
             (lazy-cat res [data])
             res)
           (let [size (first sizes)]
             (recur (lazy-cat res [(take size data)]) (rest sizes) (drop size data)))))
       (map (partial reduce reduce-fn) ,,,)))

(defn adjacent-kv-pairs
  [map key]
  {:lower (->> map
               (filter #(< (first %) key) ,,,)
               (into (sorted-map) ,,,)
               last)
   :upper (->> map
               (filter #(> (first %) key) ,,,)
               (into (sorted-map) ,,,)
               first)})

(defn interpolated-value
  "interpolate between the keys in map m for the given value key
  or if an exact match, just return the matched value in m
  m can also contain a map as values, in which case the values of that map
  are assumed to be numbers and treated as multiple results"
  [m key]
  (if (empty? m)
    nil
    (if-let [v (find m key)]
      (second v)
      ;split in lower and upper parts, lower parts
      ;will again include = to circumvent that clojure will
      ;compare for equality 20.0 = 20 => false (equivalence 20.0 == 20 => true),
      ;but the find above will use equality and thus won't match integer and
      ;floating point values
      (let [m-lower (->> m
                         (filter #(<= (first %) key) ,,,)
                         (into (sorted-map) ,,,))
            m-upper (->> m
                         (filter #(> (first %) key) ,,,)
                         (into (sorted-map) ,,,))]
        (cond
         (empty? m-lower) (-> m-upper first second)
         (empty? m-upper) (-> m-lower last second)
         :else (let [[lowerKey, lowerValue] (last m-lower)
                     [upperKey, upperValue] (first m-upper)
                     normalized-key (/ (- key lowerKey)
                                       (- upperKey lowerKey))]
                 (if (every? map? [lowerValue upperValue])
                   ;the values are actually maps
                   (into (empty lowerValue)
                         (map (fn [[lk lv] [uk uv]]
                                [lk (+ lv (* normalized-key
                                             (- uv lv)))])
                              lowerValue upperValue))
                   ;the values are normal scalars
                   (+ lowerValue (* normalized-key
                                    (- upperValue lowerValue))))))))))

(defn remove-namespace-from-keyword
  "remove the namespace from the given keyword"
  [kw]
  (-> kw name keyword))

(defmulti remove-namespace-1
  "remove namespace keywords collection just 1 level deep"
  class)

(defmethod remove-namespace-1
  clojure.lang.IPersistentMap [m]
  (reduce (fn [m [key value]]
            (if (keyword? key)
              (-> m
                  (assoc ,,, (remove-namespace-from-keyword key) value)
                  (dissoc ,,, key))
              m))
          m m))

(defn remove-namespace-from-keywords
  "remove namespace from keywords in collection deeply"
  [collection]
  (clojure.walk/prewalk #(if (keyword? %)
                           (remove-namespace-from-keyword %)
                           %)
                        collection))

(defn crop-id
  [crop]
  (str (:plot/number crop) "-" (:plot/cultivation-type crop) "-" (:plot/usage crop)))

(defn resulting-damage-compaction-depth-cm
  [plot]
  (let [{dca :plot/damage-compaction-area
         dcd :plot/damage-compaction-depth
         ca :plot/crop-area} plot]
    (when (and dca dcd (> dca (* 0.5 ca)))
      dcd)))

#_(defn awc-percent
    "available water capacity in percent (nFK Prozent)"
    [from-layer to-layer soil-moisture fc pwp]
    (let [[sum-wc sum-awc]
          (reduce (fn [[sum-wc sum-awc] i]
                    (let [[smi fci pwpi] ((juxt soil-moisture fc pwp) i)]
                      [(+ sum-wc (- smi pwpi))
                       (+ sum-awc (nFC fci pwpi))]))
                  [0. 0.] (range 0 (count soil-moisture)))]
      (max 0. (round (* 100.0 (/ sum-wc sum-awc))))))

(defn create-soil-moistures
  "create some interesting soil-moistures, function had been used in berest"
  [max-soil-depth layer-depths soil-moisture fc pwp]
  (let [;create expanded layers for all 3 inputs (1 cm layers)
        [sm fc pwp] (map #(expand-layers max-soil-depth [layer-depths %] :expand-fn /)
                         [soil-moisture fc pwp])

        ;the two functions to calculate and sum a vector of pairs of values
        ;for water content and available water content
        sum-wc-fn (partial reduce #(+ %1 (apply - %2)) 0.)
        sum-awc-fn (partial reduce #(+ %1 (apply nFC %2)) 0.)

        ;the actual function calulating the percentage for a requested layer range
        f (fn [from to]
            (let [create-pairs (partial map vector)
                  sum-wc (-> (create-pairs sm pwp)
                             vec
                             (subvec ,,, from to)
                             sum-wc-fn)
                  sum-awc (-> (create-pairs fc pwp)
                              vec
                              (subvec ,,, from to)
                              sum-awc-fn)]
              (max 0. (Math/round (* 100.0 (/ sum-wc sum-awc))))))]
    {:pNFK_0-30cm (f 0 30)
     :pNFK_30-60cm (f 30 60)
     :pNFK_0-60cm (f 0 60)}))

(defn lambda-correction
  "return the lambda correction factor for the given day of year,
  assuming right now the given day of year is without leap years"

  {:doc/origin "The expression part in the let is taken from:
   W. Mirschel et al./Ecological Modelling 81 (1995) 53-69, equation (13)
   while the doy* binding is taken from the BOWET source code (the last part can be taken from
   as well:
   Pascal code:
   IF(jahr[l] MOD 4 = 0) THEN BEGIN
   IF(tag > 305) THEN t:= tag - 305 ELSE t:= tag + 61;
   END;
   IF(jahr[l] MOD 4 > 0) THEN BEGIN
   IF(tag > 304) THEN t:= tag - 304 ELSE t:= tag + 61;
   END;
   {Die letzten 6 Anweisungen nur zur Anpassung an Programm verdtur2}
   fakt:=1 + 0.77 * SIN(0.01571 * (t - 166));"}

  [doy]
  (let [doy* (if (> doy 304)
               (- doy 304)
               (+ doy 61))]
    (+ 1 (* 0.77 (Math/sin (* 0.01571 (- doy* 166)))))))

(defn lambda-without-correction
  "create lambda without correction layer structure"
  [resulting-damage-compaction-depth-cm stt fcs-cm]
  (let [[first-30-cm below-30-cm] (map (partial apply array-map)
                                       #_[[fk-in-volP*10 lambda*100]]
                                       [[100 115, 140 68, 200 50, 240 33, 260 33,
                                         300 25, 312 25, 327 17, 356 15, 1000 15]
                                        [90 115, 120 68, 180 50, 220 33, 250 33,
                                         280 25, 344 25, 364 17, 373 15, 1000 15]])
        cm->dm #(/ % 10)
        mm7cm->volp (rcomp mm7cm->mm7dm mm7dm->volp)]
    (->> fcs-cm
         ; create cm uncorrected lambda layers
         (map-indexed
          (fn [layer-depth-cm fc]
            (/ (cond
                (and resulting-damage-compaction-depth-cm
                     (< resulting-damage-compaction-depth-cm (inc layer-depth-cm))) 1
                (<= 1121 stt 1323) 100
                (<= 713 stt 723) 5
                :else (->> (if (< layer-depth-cm 30)
                             first-30-cm
                             below-30-cm)
                           (drop-while (fn [[fc*10 _]] (>= (* (mm7cm->volp fc) 10) fc*10)) ,,,)
                           first ;first element in rest of list
                           second)) ;get lambda from pair
               100))
          ,,,)
         ; aggregate layers via choosing smallest lambda value in resulting layer
         (aggregate-layers min *layer-sizes* ,,,)
         ; divide by resulting layer-size squared in dm (to use the above dm values)
         (map #(/ %2 (* (cm->dm %) (cm->dm %))) *layer-sizes*) ,,,)))

(defn lambda
  "get layers with lambda values at a given day of year"
  [lambda-without-correction doy]
  (map (partial * (lambda-correction doy)) lambda-without-correction))

#_(defrecord Donation [day amount])

(defn donations-at
  [donations abs-day]
  (reduce
    (fn [acc {abs-start-day :donation/abs-start-day
              abs-end-day :donation/abs-end-day
              amount :donation/amount}]
      (if (= abs-day (int (/ (+ abs-start-day abs-end-day) 2)))
        (+ acc amount)
        acc))
    0 donations))

(defn db-read-irrigation-donations
  [db plot-id]
  (->> (d/q '[:find ?donation-e
              :in $ ?plot-id
              :where
              [?plot-e-id :plot/id ?plot-id]
              [?plot-e-id :plot/annuals ?pa-e-id]
              [?pa-e-id :plot.annual/donations ?donation-e]]
            db plot-id)
       (map first ,,,)
       (db/get-entities db ,,,)))

#_(defn db-create-irrigation-donation
  [datomic-connection plot-no abs-start-day abs-end-day area amount]
  (let [plot-id (ffirst (d/q '[:find ?plot :in $ ?plot-no
                               :where [?plot :plot/number ?plot-no]]
                             (d/db datomic-connection) plot-no))
        donation {:db/id (db/new-entity-id),
                  :irrigation/abs-start-day abs-start-day
                  :irrigation/abs-end-day abs-end-day
                  :irrigation/area area
                  :irrigation/amount amount}
        plot {:db/id plot-id
              :plot/irrigation-water-donations (db/get-entity-id donation)}]
    (d/transact datomic-connection (flatten [donation plot]))))

(defn read-irrigation-donations
  [db plot-id irrigation-area]
  (->> (db-read-irrigation-donations db plot-id)
       ;just include donations with more than 50% irrigated area (as we're talking always about averages)
       (filter #(> (:donation/area %) (* 0.5 irrigation-area)) ,,,)
       (map #(dissoc % :donation/area) ,,,)))


;type CodeEinheit = | PFK | PNFK | Volp | MM
;:PFK :PNFK :Volp :MM

;type IrrigationMode = | SprinkleLosses | NoSprinkleLosses
;:Irrigation-mode :Sprinkle-losses :No-sprinkle-losses

(defn user-input-fc-or-pwp-to-cm-layers
  "convert the user input field capacity or permanent wilting point to cm layers
  e.g. {30 17.7, 60 13.7, 150 15.7}
  user-input-soil-data [volP (= mm/dm) | pFK | pNFK | mm] -> [mm/cm]"
  [user-input-soil-data & {unit :unit :or {unit :volP}}]
  (let [[expand-layers*
         x->mm7cm*] (case unit
                      (:volP :pFK :pNFK) [(partial expand-layers (max-soil-depth))
                                          (rcomp volp->mm7dm mm7dm->mm7cm)]
                      :mm [#(expand-layers (max-soil-depth) %
                                           :expand-fn /
                                           :last-values-fn (fn [expanded-values last-value]
                                                             (fn [value depth] (last expanded-values))))
                           identity])]
    (->> user-input-soil-data
         sorted-unzip-map
         expand-layers*
         (map x->mm7cm* ,,,))))

(defn user-input-soil-moisture-to-cm-layers
  "convert the user input soil-moisture into the internally defined layer structure
  e.g. {30 3.4, 60 2.9, 150 3.8} :pFK
  fc, pwp [mm/cm], soil-moisture-unit [pFK | pNFK | volP | mm] -> [mm/cm]"
  [fcs-cm pwps-cm soil-moisture-unit user-input-soil-moistures]
  (let [[expand-layers*
         x->mm7cm*] (case soil-moisture-unit
                      (:volP :pFK :pNFK) [(partial expand-layers (max-soil-depth))
                                          (soil-moisture-unit {:pFK #(pFK->mm7x %1 %3)
                                                               :pNFK pNFK->mm7x
                                                               :volP #(-> %3
                                                                          volp->mm7dm
                                                                          mm7dm->mm7cm)})]
                      :mm [#(expand-layers (max-soil-depth) %
                                           :expand-fn /
                                           :last-values-fn (fn [expanded-values last-value]
                                                             (fn [value depth] (last expanded-values))))
                           (fn [_ _ sm] sm)])]
    (->> user-input-soil-moistures
         sorted-unzip-map
         expand-layers*
         (map x->mm7cm* fcs-cm pwps-cm ,,,))))

(defn user-input-ka5-soil-types-to-cm-layers
  "convert the user input KA5 soil types into the internally defined layer structure
  e.g. {30 'Ss', 60 'Hn', 150 'Ss'}"
  [user-input-ka5-soil-types]
  (->> user-input-ka5-soil-types
       sorted-unzip-map
       (expand-layers (max-soil-depth),,,)))

(defn available-plot-ids
  "get all plot ids available in the given 'db'"
  [db]
  (map first (d/q '[:find ?plot-no :in $ :where [? :plot/number ?plot-no]] db)))

(defn deep-db->plot
  "read a plot with id 'plot-id' completely given the db-value 'db'
  with associated data from year 'year'"
  [db plot-id plot-annual-id]
  (when-let [[plot-e-id annual-value-e-id]
             (first (d/q '[:find ?plot-e-id ?yv-e-id
                           :in $ ?plot-id ?plot-annual-id
                           :where
                           [?plot-e-id :plot/id ?plot-id]
                           [?plot-e-id :plot/annuals ?yv-e-id]
                           [?yv-e-id :plot.annual/id ?plot-annual-id]]
                         db plot-id plot-annual-id))]
    (let [[plot plot-av] (map (partial db/get-entity db) [plot-e-id annual-value-e-id])

          fcs-cm (-> (:plot/field-capacities plot)
                     (db/kv-entities->sorted-map ,,, :soil/upper-boundary-depth
                                                     :soil/field-capacity)
                      user-input-fc-or-pwp-to-cm-layers)
          fcs (aggregate-layers + *layer-sizes* fcs-cm)

          pwps-cm (-> (:plot/permanent-wilting-points plot)
                      (db/kv-entities->sorted-map ,,, :soil/upper-boundary-depth
                                                      :soil/permanent-wilting-point)
                       user-input-fc-or-pwp-to-cm-layers)
          pwps (aggregate-layers + *layer-sizes* pwps-cm)

          ka5-sts-cm (some-> (:plot/ka5-soil-types plot)
                              (db/kv-entities->sorted-map ,,, :soil/upper-boundary-depth
                                                              :soil/ka5-soil-type)
                              user-input-ka5-soil-types-to-cm-layers)
          ka5-sts (some->> ka5-sts-cm
                           (aggregate-layers #(conj (if (vector? %1) %1 [%1]) %2) *layer-sizes* ,,,)
                           (map #(->> %
                                      frequencies
                                      (into (sorted-map),,,)
                                      ffirst)
                             ,,,))

          sms (->> (:plot.annual/initial-soil-moistures plot-av)
                   (#(db/kv-entities->sorted-map % :soil/upper-boundary-depth
                                                 :soil/soil-moisture )
                     ,,,)
                   (user-input-soil-moisture-to-cm-layers fcs-cm pwps-cm
                                                          (->> (:plot.annual/initial-sm-unit plot-av)
                                                               remove-namespace-from-keyword)
                     ,,,)
                   (aggregate-layers + *layer-sizes* ,,,))

          lwc (lambda-without-correction
                (resulting-damage-compaction-depth-cm plot)
                (-> plot :plot/stt :soil.stt/key)
                fcs-cm)

          ;read a fallow "crop" to be used with this plot
          fallow (data/db->crop-by-name db 0 :cultivation-type 0 :usage 0)

          plot-av* (->> plot-av
                        (data/deep-entity->map db ,,,)
                        (clojure.walk/postwalk (fn [item]
                                                 (if (vector? item)
                                                   (let [[kw crop-entity] item]
                                                     (if (= kw :crop.instance/template)
                                                       [kw (data/crop-entity->crop crop-entity)]
                                                       item))
                                                   item))
                          ,,,))]
      (-> (data/deep-entity->map db plot)
          (dissoc ,,, :db/id :plot/annuals)
          (merge ,,, (dissoc plot-av* :db/id))
          (assoc ,,, :plot.annual/initial-soil-moistures sms
                     :plot/field-capacities fcs
                     :plot/permanent-wilting-points pwps
                     :plot/ka5-soil-types ka5-sts
                     :lambda-without-correction lwc
                     :fallow fallow)))))

#_(defn db-store-initial-soil-moisture
  [datomic-connection plot-no depths soil-moistures units]
  (let [plot-id (ffirst (d/q '[:find ?plot :in $ ?plot-no
                               :where [?plot :plot/number ?plot-no]]
                             (d/db datomic-connection) plot-no))
        entities (db/create-entities {:soil/upper-boundary-depth depths
                                      :soil/soil-moisture soil-moistures
                                      :soil/soil-moisture-unit units})
        plot {:db/id plot-id
              :plot/user-soil-data (db/get-entity-ids entities)}]
    (d/transact datomic-connection (flatten [entities plot]))))

(defn glugla
  "Calculates the excess water in a soil-layer given the initial water content, the
  infiltration into that layer and the lambda value for the layer, by default
  for one day, but optionally for a time slice of dti days:
  ni* [mm] = Ni* = Ni [mm/d] - Vi [mm/d] = precipitation - evaporation
  dti [d] = delta ti = length of time slice i [d]
  wia [mm] = WiA = water content at the start of time slice i
  ni* [mm] = Ni* = Ni [mm/d] - Vi [mm/d] = precipitation - evaporation
  lai [1/(mm*d)] = lambda i = parameter of the water movement in the soil during time slice i
  wie [mm] = WiE = water content at the end of time slice i = final excess water"
  [wia ni* lai & {dti :dti :or {dti 1}}]

  {:doc/origin "Gerhard Glugla, Albrecht-Thaer-Archiv, Arbeiten aus den Gebieten Bodenkunde,
   Pflanzenernährung, Düngung, Acker- und Pflanzenbau, 1969, 13. Band, Akademie-Verlag Berlin,
   Berechnungsverfahren zur Ermittlung des aktuellen Wassergehalts und Gravitationswasserabflusses
   im Boden, Seite 374, Abb. 2"}

  (cond
   ;more water will fit into the current layer (below infiltration barrier)
   (neg? wia) (let [wie (+ (* ni* dti) wia)]
                (if (and (pos? ni*)
                         (pos? wie))
                  (let [dsti1 (- (/ wia
                                    ni*))
                        ais (- (Math/exp (* -2 (Math/sqrt (* lai ni*)) (- dti dsti1))))]
                    (/ (* (Math/sqrt (/ ni*
                                        lai))
                          (+ 1 ais))
                       (- 1 ais)))
                  wie))
   ;current layer already above infiltration barrier
   :else (cond
          (zero? ni*) (/ wia
                         (+ 1 (* lai wia dti)))
          ;Entzug
          (neg? ni*)(let [abs-ni* (- ni*)
                          n7l (Math/sqrt (/ abs-ni*
                                            lai))
                          l*n (Math/sqrt (* abs-ni* lai))
                          dti1 (/ (Math/atan (/ wia
                                                n7l))
                                  l*n)]
                      (if (> dti1 dti)
                        (let [bi (Math/tan (* l*n dti))]
                          (/ (* n7l (- wia (* n7l bi)))
                             (+ n7l (* wia bi))))
                        (* ni* (- dti dti1))))
          ;(pos? ni*) // kein Entzug
          :else (let [n7l (Math/sqrt (/ ni*
                                        lai))
                      l*n (Math/sqrt (* lai ni*))
                      ai (* (/ (- wia n7l)
                               (+ wia n7l))
                            (Math/exp (* -2 l*n dti)))]
                  (/ (* n7l (+ 1 ai))
                     (- 1 ai))))))

(defn glugla*
  "same as glugla, but immediately calculates also the infiltration into the next layer"
  [initial-excess-water infiltration-from-layer-above lambda & {dti :delta-t-i :or {dti 1}}]
  (let [few (glugla initial-excess-water infiltration-from-layer-above lambda :dti dti)]
    {:final-excess-water few
     :infiltration-into-next-layer (+ initial-excess-water infiltration-from-layer-above (- few))}))

(defn interception
  [precipitation evaporation donation sprinkle-loss-factor transpiration-factor]
  (let [null5 0.5
        null2 0.2
        tf (max 1 transpiration-factor)

        ;Berechnung fuer natuerlichen Regen
        tin (+ null5 (* (- evaporation 1) tf null2))
        [interception-precipitation, pet] (if (> precipitation 0)
                                            [tin, (- evaporation (* tin null5))]
                                            [0, evaporation])

        ;Berechnung fuer Zusatzregen/Spruehverluste
        [donation*
         interception-irrigation
         sprinkle-loss
         pet*]
        (if (and donation (> donation 0)) ;;was (> sprinkler-donation 1), don't know why
          (let [ii (* 0.6 tin (+ 1 (* donation 0.05)))
                sl (* (+ 1 (* (- evaporation 2.5) null2)) sprinkle-loss-factor donation)]
            (if (> precipitation 0)
              [donation ii sl (- evaporation (* (+ ii interception-precipitation) null5))]
              [donation ii sl (- evaporation (* ii 0.75))]))
          [0 0 0 pet])]
    {:pet (max 0 pet*),
     :effective-precipitation (- precipitation interception-precipitation),
     :effective-donation (- donation* interception-irrigation sprinkle-loss),
     :effective-donation-uncovered (- donation* sprinkle-loss)}))

(defn uncovered-water-abstraction-fraction
  "Get fraction of water-abstraction on uncovered soil for given layer i when using maximal m equal sized layers
  the function will use by default the curvature parameter z with value 0.05 which fits best the value for the
  first two layers in the original BEREST table [:0-10cm 0.625, :10-20cm 0.3, :30-60cm 0.075].
  The third layer (30-60cm) isn't matched exactly, but it's the layer with the smallest water-abstraction
  and very close (0.08145 vs 0.075), but the integral over all the layers is nevertheless 1 and maybe
  the original values have been choosen to get rounded but close to the functions results values."
  [m i & {:keys [z] :or {z 0.05}}]
  {:doc/origin "R. Koitzsch, Zeitschrift für Meteorologie Band 27 Heft 5,
   Schätzung der Bodenfeuchte mit einem Mehrschichtenmodell, equation (9)"}
  (/ (- (* (+ z 1)
           (Math/log (/ (+ (* m z) i)
                        (+ (* m z) i -1))))
        (/ 1 m))
     (- (* (+ z 1)
           (Math/log (/ (+ z 1)
                        z)))
        1)))

(defn covered-water-abstraction-fraction
  "Get fraction of water-abstraction on plant covered soil for given layer i when using maximal n equal sized layers."
  [n i]
  {:doc/origin "R. Koitzsch, Zeitschrift für Meteorologie Band 27 Heft 5,
   Schätzung der Bodenfeuchte mit einem Mehrschichtenmodell, equation (6)"}
  (/ (- 2 (/ (- (* 2 i) 1)
             n))
     n))

(defn complement-layers
  [no-of-layers with-value incomplete-layers]
  (take no-of-layers (concat incomplete-layers (repeat with-value))))

(defn unreduced-water-abstractions
  "returns a sequence of soil-depth-cm layers with water-abstraction values for covered or uncovered case
  for the given maximum depth in cm"
  [water-abstraction-fraction-fn max-depth-cm]
  {:doc/origin "used to be f1-koitzsch in BEREST90"}
  (->> (for [i (range 1 (inc max-depth-cm))]
         (water-abstraction-fraction-fn max-depth-cm i))
       (complement-layers (max-soil-depth) 0 ,,,)))

(def uncovered-unreduced-water-abstractions
  (unreduced-water-abstractions uncovered-water-abstraction-fraction 60))

(def covered-unreduced-water-abstractions
  (partial unreduced-water-abstractions covered-water-abstraction-fraction))

(defn gi-koitzsch
  "calculate water abstraction for a given maximum extraction depth and the give
  reduction factors
  note: code still contains the uncovered case, but that code has actually moved
  one level up to the caller of gi-koitzsch, because the extraction depth doesn't make
  much sense for uncovered soil and has been originally defined to be 60cm, which is
  constant throughout the program, whereas in the uncovered case the there is a maximum
  rooting (extraction) depth which can be fully used, but will only be so, if
  given the reduction factors applied to the layers gives the the largest abstraction, else
  shallower extraction depth will be used"
  [extraction-depth-cm reduction-factors]
  (let [;we search within the layers above and equal to extraction-depth-cm
        search-layer-depths (take-while (partial >= extraction-depth-cm) (layer-depths))
        ;create a list of [f1-koitzsch-result] for every possible layer depth in use
        ffss (for [max-depth search-layer-depths]
              (->> (covered-unreduced-water-abstractions max-depth)
                   (aggregate-layers + *layer-sizes* ,,,)))
        ;calculate the gj and store the sum of all layers to find out the maximum later
        gis (map (fn [depth-cm ffs]
                   (let [gj (bu/dot-mult reduction-factors ffs)
                         rij (bu/sum gj)]
                     {:depth-cm depth-cm, :rij rij, :gj gj})) search-layer-depths ffss)
        ;sort gis according to ascending rij and then descending depth-cm
        gis-sorted (sort-by identity
                            (fn [{l-rij :rij, l-depth :depth-cm}
                                 {r-rij :rij, r-depth :depth-cm}]
                              (if (= l-rij r-rij)
                                (> l-depth r-depth)
                                (> l-rij r-rij)))
                            gis)]
    ;just use values only below 50cm, take the first one and extract the gj and complement with zeros below extraction depth
    (->> gis-sorted
         first
         :gj
         (complement-layers (no-of-soil-layers) 0 ,,,))))

(defn capillary-rise-barrier
  "calculate capillary rise barrier for a layer with size x-dm based on the field capacity of a x-dm layer"
  [fc-xdm layer-size-dm]
  (let [fc-1dm (/ fc-xdm
                  layer-size-dm)]
    (* (+ fc-1dm 40.667 (* fc-1dm -0.408))
       layer-size-dm)))

(defn infiltration-barrier
  "calculates at which soil-moisture level in soil-layer infiltration into next layer will happen"
  [fk pwp abs-current-day layer-depth-cm]
  (let [barriers (if (<= layer-depth-cm 30)
                   (sorted-map 80 11
                               140 7)
                   (sorted-map 100 10
                               200 8))
        vs (* (interpolated-value barriers abs-current-day) 0.1)
        vs* (let [pwfk (+ (/ pwp fk) -1 vs)]
              (if (and (< vs 1.)
                       (pos? pwfk))
                (+ vs (/ (* (- 1 vs) pwfk 0.95)
                         (- 0.66 0.3)))
                vs))]
    (+ (* (nFC fk pwp) vs*)
       pwp)))

(defn pwp4p
  [fc pwp]
  (- pwp (* 0.04 (nFC fc pwp))))

(defn uncovered-water-abstraction-reduction-factors
  "factors reducing the water abstraction on uncovered ground,
  as there is no crop on soil, can only depend on soil properties"
  [fcs pwps soil-moistures]
  (map (fn [fc pwp sm]
         (let [pwp4p* (pwp4p fc pwp)
               r (if (> sm pwp4p*)
                   (min (/ (- sm pwp4p*)
                           (- fc pwp4p*))
                        1)
                   0)]
           (* r r)))
       fcs pwps soil-moistures))

(defn covered-water-abstraction-reduction-factors
  [extraction-depth-cm fcs pwps soil-moisture pet]
  (map (fn [layer-size-cm depth-cm fc pwp sm]
         (let [pwp4p* (pwp4p fc pwp)]
           (if (and (<= depth-cm extraction-depth-cm)
                    (< sm fc))
             (let [nfc (nFC fc pwp)
                   fc-dm (/ fc layer-size-cm 10)
                   xsm (if (or (< fc-dm 10)
                               (> fc-dm 46))
                         (+ (* nfc 0.81) pwp)
                         (+ (* (+ 67.77
                                  (* 3.14 fc-dm)
                                  (* -0.2806 fc-dm fc-dm)
                                  (* 0.008131 fc-dm fc-dm fc-dm)
                                  (* -0.0000735 fc-dm fc-dm fc-dm fc-dm))
                               nfc 0.01)
                            pwp))
                   rk (if (> pet 3)
                        (* (- fc xsm) 0.0666)
                        (* (- xsm pwp) 0.3333))
                   sm-crit (+ xsm (* (- pet 3) rk))]
               (if (< sm sm-crit)
                 (let [pwp12p (- pwp (* 0.12 nfc))]
                   (cond
                    (< pwp sm) (/ (- sm pwp12p)
                                  (- sm-crit pwp12p))
                    (< pwp4p* sm) (let [sreb 3] ; { sreb=(pwp-pw7)/(pwp-pw9)=3.0 }
                                    (* (/ (- sm pwp4p*)
                                          (- sm-crit pwp12p))
                                       sreb))
                    :else 0))
                 1))
             1)))
       *layer-sizes* (layer-depths) fcs pwps soil-moisture))

(defn capillary-rise
  "calculate capillary rise according to a simple algorithm taken from MONICA
  -> only one layer per day will be filled and only the closest one to the groundwater table
  with free available water below 70% of field-capacity - permanent wilting point"
  [groundwater-level-cm fcs pwps capillary-rise-rates soil-moistures]
  (let [capillary-rise-rates* (map (partial min 10.0) capillary-rise-rates)
        capillary-water-70 (map (fn [fc pwp] (* (- fc pwp) 0.7)) fcs pwps)
        available-water (map (fn [sm pwp] (max 0 (- sm pwp))) soil-moistures pwps)
        #__ #_(println "capillary-rise-rates*: " capillary-rise-rates*
                   " capillary-water-70: " capillary-water-70
                   " available-water: " available-water)

        data (map vector (layer-depths) available-water capillary-water-70
                  capillary-rise-rates* soil-moistures)]
    (->> data
         reverse
         (reduce (fn [{:keys [sms applied-water-to-closest-matching-layer?] :as acc}
                      [depth-cm available-water capillary-water-70 rate sm]]
                   (if (and (not applied-water-to-closest-matching-layer?)
                            (or (not groundwater-level-cm)
                                (<= depth-cm groundwater-level-cm))
                            (< available-water capillary-water-70))
                     {:sms (cons (+ sm rate) sms) :applied-water-to-closest-matching-layer? true}
                     (update-in acc [:sms] conj sm)))
                 {:sms '() :applied-water-to-closest-matching-layer? false}
                 ,,,)
         :sms)))

(defn calc-soil-moisture*
  "calculate soil-moisture for one day"
  [{:keys [extraction-depth-cm cover-degree abs-day
           fcs pwps lambdas capillary-rise-rates
           soil-moisture-prognosis?
           evaporation groundwater-level-cm
           technology-type
           donation technology-outlet-height]
    :as input}
   pet soil-moistures daily-precipitation-and-donation]
  #_(println "calc-soil-moisture* input: " (pr-str input))
  (let [drip-donation (when (= technology-type :technology.type/drip)
                        donation)
         soil-moistures* (if drip-donation
                          (map (fn [sm depth]
                                 (if (<= technology-outlet-height depth)
                                   (+ sm drip-donation)
                                   sm))
                               soil-moistures (layer-depths))
                          soil-moistures)

        #__ #_(println "daily-precip: " daily-precipitation-and-donation
                   " soil-moistures: " soil-moistures*
                   " capillary-rise-rates: " capillary-rise-rates)

         sms+surface-water (concat [(+ daily-precipitation-and-donation (first soil-moistures*))]
                                  (rest soil-moistures*))

        ;for at least partly uncovered ground
        uncovered-water-abstractions
        (if (>= cover-degree 99/100)
          (repeat (no-of-soil-layers) 0)
          ;calculate same as gi-koitzsch for a depth of 60cm but uncovered soil
          (->> uncovered-unreduced-water-abstractions
               (aggregate-layers + *layer-sizes* ,,,)
               (bu/dot-mult (uncovered-water-abstraction-reduction-factors fcs pwps sms+surface-water) ,,,)
               (bu/s-mult evaporation ,,,)))

        ;for at least partly covered ground
        [aet, water-abstractions]
        (if (<= cover-degree 1/100)
          [0, uncovered-water-abstractions]
          (let [extraction-depth-cm* (if soil-moisture-prognosis?
                                       (min extraction-depth-cm 60)
                                       extraction-depth-cm)
                rfs (if (> cover-degree 1/100)
                      (covered-water-abstraction-reduction-factors extraction-depth-cm fcs pwps sms+surface-water pet)
                      (repeat (no-of-soil-layers) 1))
                gi (gi-koitzsch extraction-depth-cm* rfs)]
            [(bu/sum (bu/s-mult pet gi)),
             (bu/dot-add (bu/s-mult (- 1 cover-degree) uncovered-water-abstractions)
                         (bu/s-mult (* pet cover-degree) gi))]))

        {groundwater-infiltration :infiltration-into-next-layer
         soil-moistures** :soil-moistures}
        (->> soil-moistures*
             ;combine a few more needed inputs to infiltration calculation
             (map vector lambdas water-abstractions *layer-sizes* (layer-depths) fcs pwps ,,,)
             ;calculate the infiltration top down layer by layer, transporting excess water down
             ;and building up the soil layers as we go down
             (reduce (fn [{infiltration-from-prev-layer :infiltration-into-next-layer
                           sms :soil-moistures}
                          [lambda water-abstraction layer-size-cm depth-cm fc pwp sm]]
                       (if (or (not groundwater-level-cm)
                               (<= depth-cm groundwater-level-cm))
                         ;above that barrier the water will start to infiltrate to the next layer
                         (let [inf-barrier (infiltration-barrier fc pwp abs-day depth-cm)

                               ;in the next steps we basically care just about the excess water
                               ;not about the layers full water content, so we calculate
                               ;just the difference (positive or negative) to the infiltration barrier
                               ;(in case of the first layer, possibly including todays precipitation)
                               initial-excess-water (- sm inf-barrier)

                               net-infiltration-from-above-layer (- infiltration-from-prev-layer water-abstraction)

                               ;the excess water after calculation of the infiltration to the next layer
                               {:keys [final-excess-water
                                       infiltration-into-next-layer]}
                               (glugla* initial-excess-water net-infiltration-from-above-layer lambda)

                               ;add the (positive/negative) excess water back to the infiltration barrier
                               ;to obtain the actual water content now in this layer
                               ;(after water could infiltrate to the next layer)
                               sm* (max (+ inf-barrier final-excess-water) (pwp4p fc pwp))]
                           {:infiltration-into-next-layer infiltration-into-next-layer,
                            :soil-moistures (conj sms sm*)})
                         {:infiltration-into-next-layer 0
                          :soil-moistures (conj sms (capillary-rise-barrier fc (/ layer-size-cm
                                                                                  10)))}))
                     {:infiltration-into-next-layer daily-precipitation-and-donation
                      :soil-moistures []}
                     ,,,))

        ;calculate the capillary rise if groundwater table is high enough
        soil-moistures*** (if capillary-rise-rates
                            (capillary-rise groundwater-level-cm fcs pwps capillary-rise-rates soil-moistures**)
                            soil-moistures**)

        ; VERTIKALER AUSGLEICH BEI UEBERFEUCHTUNG
        [_ soil-moistures****] (->> soil-moistures***
                                   ;combine soil-moistures with fc and *layer-sizes* for reduce function below
                                   (map vector fcs *layer-sizes* ,,,)
                                   ;reverse, to go from bottom to top layer
                                   reverse
                                   ;transport excess water from lower layers to top layers
                                   (reduce (fn [[excess-water sms] [fc layer-size-cm sm]]
                                             (let [cr-barrier (capillary-rise-barrier fc (/ layer-size-cm 10))
                                                   sm* (+ sm excess-water)
                                                   excess-water* (max 0 (- sm* cr-barrier))]
                                               [excess-water* (cons (- sm* excess-water*) sms)]))
                                           [0 '()]
                                     ,,,))]
    {:aet aet
     :soil-moistures soil-moistures****
     :groundwater-infiltration groundwater-infiltration}))

(defn calc-soil-moisture
  "calculate soil-moisture for the given input"
  [{:keys [qu-sum-deficits qu-sum-targets soil-moistures]
    :as result-accumulator}
   {:keys [abs-day rel-dc-day
           donation technology-type technology-outlet-height technology-sprinkle-loss-factor
           evaporation precipitation
           cover-degree qu-target
           transpiration-factor]
    :as input}]
  #_(println "evap: " evaporation
           " precip: " precipitation
           " donation: " donation
           " technology-sprinkle-loss-factor: " technology-sprinkle-loss-factor
           " result-acc: " #_(pr-str result-accumulator))
  (let [{:keys [pet
                effective-precipitation
                effective-donation
                effective-donation-uncovered]}
        (interception precipitation evaporation
                      (when (or (= technology-type :technology.type/sprinkler)
                                ;drip irrigation outlet height shouldn't actually be larger than 0
                                (and (= technology-type :technology.type/drip)
                                     (>= technology-outlet-height 0)))
                        donation)
                      technology-sprinkle-loss-factor
                      transpiration-factor)

        pet* (if (< cover-degree 1/1000)
               0
               (* pet transpiration-factor))

        ;should include above ground drip irrigation and sprinkler irrigation donations
        daily-precipitation-and-donation
        (+ (* (+ effective-precipitation effective-donation)
              cover-degree)
           (* (+ precipitation effective-donation-uncovered)
              (- 1 cover-degree)))

        {aet :aet
         soil-moistures* :soil-moistures
         groundwater-infiltration :groundwater-infiltration}
        (calc-soil-moisture* input pet* soil-moistures daily-precipitation-and-donation)

        aet7pet (cond
                 (< cover-degree 1/1000) 0
                 (> pet* 1/100) (/ aet pet*)
                 :else 1)

        ;_ (println "abs-day: " abs-day " aet7pet: " aet7pet)
        ]

    {:abs-day abs-day
     :rel-dc-day rel-dc-day
     :donation donation
     :effective-precipitation effective-precipitation
     :effective-donation effective-donation
     :effective-donation-uncovered effective-donation-uncovered
     :aet aet
     :pet pet*
     :aet7pet aet7pet
     :qu-target qu-target
     :qu-deficit #_(- qu-target aet7pet) (if (< aet7pet qu-target)
                                          (- qu-target aet7pet)
                                          0)
     :qu-sum-deficits #_(+ qu-sum-deficits (- qu-target aet7pet)) (if (< aet7pet qu-target)
                                                                    (+ qu-sum-deficits (- qu-target aet7pet))
                                                                    qu-sum-deficits)
     :qu-sum-targets (+ qu-sum-targets qu-target)
     :soil-moistures soil-moistures*
     :groundwater-infiltration groundwater-infiltration}))

(defn shift-crop-curves
  [crop & {:keys [by after]}]
  #_(println "(shift-crop-curves :by " by " after: " after)
  (let [curves (select-keys crop [:crop/rel-dc-day-to-cover-degrees
                                  :crop/rel-dc-day-to-quotient-aet-pets
                                  :crop/rel-dc-day-to-transpiration-factors
                                  :crop/rel-dc-day-to-extraction-depths])
        ;_ (println "curves: ")
        ;_ (pp/pprint curves)

        shift-f (fn [curve]
                  (let [am (into (array-map)
                                 (for [[rel-dc-day v] curve]
                                   (if (>= rel-dc-day after)
                                     [(+ rel-dc-day by) v]
                                     [rel-dc-day v])))
                        am* (reduce (fn [[m ram] [rel-dc-day v]]
                                      (if (apply < (keys ram))
                                        [(assoc m rel-dc-day v) (rest ram)]
                                        [m (rest ram)]))
                                    [{} am] (seq am))]
                    (into (sorted-map) am*)))
        curves* (fmap shift-f curves)
        ;_ (println "curves*: ")
        ;_ (pp/pprint curves*)
        ]
    (apply assoc crop (flatten (seq curves*)))))

(defn dc-to-abs+rel-dc-day-from-crop-instance-dc-assertions
  "create a dc to abs+rel-dc-day map from the data given with crop-instance
  (dc-assertions and the crop template, thus the dc-to-rel-dc-day curve)"
  [crop-instance]
  (when-let [dc-assertions (->> crop-instance
                                :crop.instance/dc-assertions
                                (sort-by :assertion/abs-assert-dc-day ,,,)
                                not-empty)]
    #_(println "crop")
    #_(pp/pprint (:crop.instance/template crop-instance))
    #_(println "dc-assertions: " (pr-str dc-assertions))
    (let [{dc* :assertion/assert-dc
           abs-dc-day* :assertion/abs-assert-dc-day} (first dc-assertions)
          ;_ (println "dc*: " dc* " abs-dc-day*: " abs-dc-day*)

          sorted-dc-to-rel-dc-days (->> crop-instance
                                        :crop.instance/template
                                        :crop/dc-to-rel-dc-days
                                        (into (sorted-map) ,,,))
          ;_ (println "sorted-dc-to-rel-dc-days: " (pr-str sorted-dc-to-rel-dc-days))

          rel-dc-day* (interpolated-value sorted-dc-to-rel-dc-days dc*)
          sorted-dc-to-rel-dc-days* (assoc sorted-dc-to-rel-dc-days dc* rel-dc-day*)

          sorted-initial-dc-to-abs+rel-dc-day
          (fmap (fn [rel-dc-day]
                  {:abs-dc-day (+ abs-dc-day* (- rel-dc-day rel-dc-day*))
                   :rel-dc-day rel-dc-day})
                sorted-dc-to-rel-dc-days*)
          ;_ (println "initial: ")
          ;_ (pp/pprint sorted-initial-dc-to-abs+rel-dc-day)
          ]
      (reduce (fn [[crop-instance sorted-dc-to-days] {dc* :assertion/assert-dc
                                                      abs-dc-day* :assertion/abs-assert-dc-day}]
                (let [;get a new days pair, either from map (should be the only case) or interpolate new
                      {:keys [abs-dc-day rel-dc-day]} (fmap (rcomp bu/round int)
                                                            (or (get sorted-dc-to-days dc*)
                                                                (interpolated-value sorted-dc-to-days dc*)))
                      ;_ (println "abs-dc-day: " abs-dc-day " rel-dc-day: " rel-dc-day)

                      ;that many days the assertion shifts the reported dc state
                      new-delta (- abs-dc-day* abs-dc-day)
                      ;_ (println "dc*: " dc* " abs-dc-day*: " abs-dc-day* " new-delta: " new-delta)

                      rel-dc-day* (+ rel-dc-day new-delta)

                      ;directly store the asserted dc state, because that's what the user reported
                      sorted-dc-to-days* (assoc sorted-dc-to-days dc* {:abs-dc-day abs-dc-day*
                                                                       :rel-dc-day rel-dc-day*})

                      ;apply the new delta to all future dc-to-days pairs, because the future
                      ;(regarding the asserted abs-day of an dc state) represents just average possible
                      ;time ranges for the dcs
                      ;also if a past dc (regarding the assert dc*) has due to the new delta
                      ;a larger abs-dc-day, this mapping will be removed
                      sorted-dc-to-days** (->> sorted-dc-to-days*
                                               (map (fn [[dc {:keys [abs-dc-day rel-dc-day]} :as key-value]]
                                                      ;move everyting after assert dc* by new-delta days
                                                      (if (> dc dc*)
                                                        [dc {:abs-dc-day (+ abs-dc-day new-delta)
                                                             :rel-dc-day (+ rel-dc-day new-delta)}]
                                                        ;if there's an assertion before that has due to new-delta
                                                        ;a larger abs-dc-day than the assert abs-dc-day*
                                                        ;remove (skip) this assertion
                                                        (when (<= abs-dc-day abs-dc-day*)
                                                          key-value))),,,)
                                               (into (sorted-map),,,))
                      ;_ (println "sorted-dc-to-days**")
                      ;_ (pp/pprint sorted-dc-to-days**)

                      crop-instance* (assoc crop-instance :crop.instance/template
                                            (shift-crop-curves (:crop.instance/template crop-instance)
                                                               :by new-delta
                                                               :after rel-dc-day))

                      ; also update the dc-to-rel-dc-day in the crop-instance
                      crop-instance** (assoc-in crop-instance* [:crop.instance/template :crop/dc-to-rel-dc-days]
                                                (fmap :rel-dc-day sorted-dc-to-days**))

                      ;_ (println "crop**")
                      ;_ (pp/pprint (:crop.instance/template crop-instance**))
                      ]
                  [crop-instance** sorted-dc-to-days**]))
              [crop-instance sorted-initial-dc-to-abs+rel-dc-day] (rest dc-assertions)))))

(defn dc-to-abs+rel-dc-day-from-plot-dc-assertions
  "create a dc to abs-dc-day map for all crops in sequence of dc-assertions"
  [crop-instances]
  (->> crop-instances
       (map (fn [crop-instance]
              (dc-to-abs+rel-dc-day-from-crop-instance-dc-assertions crop-instance)
              #_[crop-instance
               (dc-to-abs+rel-dc-day-from-crop-instance-dc-assertions crop-instance)])
            ,,,)
       (into {} ,,,)))

(defn- index-localized-crop-instance-curves-by-abs-dc-day
  "transform result to map like form indexed by abs-dc-day for sorting and processing"
  [localized-crop-instance-curves]
  (map (fn [[crop-instance dc-map]]
         (map (fn [[dc {:keys [abs-dc-day rel-dc-day]}]]
                [abs-dc-day {:dc dc
                             :rel-dc-day rel-dc-day
                             :crop-instance crop-instance}])
              dc-map))
       localized-crop-instance-curves))

(defn- merge-abs-dc-day-to-crop-data-maps
  "merge all crops/abs-dc-days as pre-stage to get final map with correct order in year"
  [abs-dc-day-to-crop-data]
  (reduce (fn [m crop-map]
            (into m
                  (for [[abs-dc-day data-map] crop-map]
                    (assoc m abs-dc-day
                      (if-let [val (get m abs-dc-day)]
                        (if (vector? val)
                          (conj val data-map)
                          [val data-map])
                        data-map)))))
          (sorted-map) abs-dc-day-to-crop-data))

(defn- calculate-final-abs-dc-to-crop-data-map
  "calculate order given a list of merged abs-dc-day-to-crop-data"
  [merged-abs-dc-day-to-crop-data-maps]
  (->> merged-abs-dc-day-to-crop-data-maps
       (reduce (fn [{:keys [m last-crop-instance crop-canceled?]}
                    [abs-dc-day data-map?s]]
                 (let [v? (vector? data-map?s)
                       lci (if last-crop-instance
                             last-crop-instance
                             ;if is vector simply take first crop-instance, other decision could be implemented
                             (:crop-instance ((if v? first identity) data-map?s)))]
                   (if v?
                     ;we got a cancelation of the current crop, but have to decide which
                     ;to choose now
                     (->> data-map?s
                          ;ignore the data belong to last crop instance
                          (filter #(not= lci (:crop-instance %)) ,,,)
                          ;for now simply take first from list (other decision could be implemented)
                          first
                          ;create mapping to data for current abs-dc-day
                          (assoc m abs-dc-day ,,,)
                          ;create accumulator for next reduce call
                          (#(hash-map :m %
                                      :last-crop-instance (:crop.instance %)
                                      :crop-canceled? true) ,,,))
                     ;ignore crop/crop-data if its from previous crop
                     ;and only if crop has been canceled before by other crop
                     {:m (if (or (= lci (:crop-instance data-map?s))
                                 (not crop-canceled?))
                           (assoc m abs-dc-day data-map?s)
                           m)
                      :last-crop-instance lci
                      :crop-canceled? crop-canceled?})))
               {:m (sorted-map)
                :last-crop-instance nil
                :crop-canceled? false} ,,,)
       :m))

(defn abs-dc-day->crop-instance
  "given an abs-dc-day return the crop-instance being used and
  the rel-dc-day for this crop-instance:
  assumes that
  a) dc = 1 means seeding, if there's no dc = 1, then is a winter crop, except
  if the crop has a previous crop, then a dc > 1 means the crop stop breaks the previous crop
  b) there will always be a harvesting (= last dc) step, thus after this step there
  is always fallow unless another crop follows with a dc > 1 (see a)"
  [fallow abs-dc-day-to-crop-instance-data abs-dc-day]
  (let [fallow* {:dc 0
                 :rel-dc-day 1
                 :crop fallow}

        ;first-abs-dc-day (ffirst abs-dc-day-to-crop-instance-data)
        ]
    (if-let [{dc :dc
              rel-dc-day :rel-dc-day
              {crop :crop.instance/template
               :as crop-instance} :crop-instance}
             (get abs-dc-day-to-crop-instance-data abs-dc-day)]
      {:dc dc
       :rel-dc-day rel-dc-day
       :crop crop
       :crop-instance crop-instance}
      (let [{[l-abs-dc-day
              {l-dc :dc
               l-rel-dc-day :rel-dc-day
               {l-crop :crop.instance/template} :crop-instance
               :as lower}] :lower
             [u-abs-dc-day
              {u-dc :dc
               u-rel-dc-day :rel-dc-day
               {u-crop :crop.instance/template} :crop-instance
               :as upper}] :upper}
            (adjacent-kv-pairs abs-dc-day-to-crop-instance-data abs-dc-day)]
        (cond
         ;before summer crop, just fallow, but before winter crop = winter crop
         (and (not lower) upper) (if (> u-dc 1)
                                   {:dc u-dc
                                    :rel-dc-day u-rel-dc-day
                                    :crop u-crop
                                    :crop-instance (:crop-instance upper)}
                                   fallow*)
         ;after last crop just fallow
         (and lower (not upper)) fallow*
         (and lower upper) (if
                             ;if next=prev crop, just interpolate
                             (or (= l-crop u-crop)
                                 ;if next crop is a different crop and next crop has
                                 ;a dc > 1, thus already after seeding
                                 ;keep on doing the previous crop, until the next crop
                                 ;comes, because the next crop will stop break the previous
                                 ;crop
                                 (and (not= l-crop u-crop)
                                      (> u-dc 1)))
                             {:dc l-dc
                              :rel-dc-day (+ l-rel-dc-day (- abs-dc-day l-abs-dc-day))
                              :crop l-crop
                              :crop-instance (:crop-instance lower)}
                             ;if next crop is a different crop, fallow if
                             ;the next crop's dc is 1 => there new crop will start
                             fallow*)
         :else nil)))))

(defn capillary-rise-rates
  [ka5-soil-types rooting-depth-cm]
  #_(println "ka5-soil-types: " ka5-soil-types " rooting-depth-cm: " rooting-depth-cm)
  (when ka5-soil-types
    (let [rr-per-soil-type (->> (d/q '[:find ?soil-type ?rate
                                       :in $ [?soil-type ...] ?distance-dm
                                       :where
                                       [?e :soil.type.ka5/name ?soil-type]
                                       [?e :soil.type.ka5/capillary-rise-rates ?rates]
                                       [?rates :soil.type.ka5/distance-to-groundwater-table ?distance-dm]
                                       [?rates :soil.type.ka5/capillary-rise-rate ?rate]]
                                     (db/current-db) ka5-soil-types (int (/ rooting-depth-cm 10)))
                                (into {},,,))

          #__ #_(println "rooting-depth-cm: " rooting-depth-cm
                     " rr-per-soil-type: " rr-per-soil-type
                     " ka5-soil-types: " ka5-soil-types
                     " -> " (->> ka5-soil-types
                                 (map rr-per-soil-type ,,,)
                                 (map (fnil identity 0.0) ,,,)))
          ]
      (->> ka5-soil-types
           (map rr-per-soil-type ,,,)
           (map (fnil identity 0.0) ,,,)))))

(defn base-input-seq
  "create a input sequence for soil-moisture calculations
  - takes into account dc assertions which are available in plot map
  - lazy sequence as long as weather is available"
  [plot sorted-weather-map donations technology-type]
  (when (some-> sorted-weather-map seq)
    (let [abs-dc-day-to-crop-instance-data
          (->> (:plot.annual/crop-instances plot)
             dc-to-abs+rel-dc-day-from-plot-dc-assertions
             index-localized-crop-instance-curves-by-abs-dc-day
             merge-abs-dc-day-to-crop-data-maps
             calculate-final-abs-dc-to-crop-data-map)]
    (for [abs-day (range (ffirst sorted-weather-map) (-> sorted-weather-map rseq ffirst inc))
          :let [weather (sorted-weather-map abs-day)]
          :while weather]
      (let [{:keys [dc rel-dc-day crop crop-instance]}
            (abs-dc-day->crop-instance (:fallow plot) abs-dc-day-to-crop-instance-data abs-day)

            prev-day-cover-degree (interpolated-value (:crop/rel-dc-day-to-cover-degrees crop)
                                                      (dec rel-dc-day))

            cover-degree (interpolated-value (:crop/rel-dc-day-to-cover-degrees crop) rel-dc-day)
            extraction-depth-cm (if (<= cover-degree 1/1000)
                                  0
                                  (int (bu/round (* 10 (interpolated-value (:crop/rel-dc-day-to-extraction-depths crop) rel-dc-day)))))
            groundwater-level-cm (:plot/groundwaterlevel plot)
            ]
        {:dc dc
         :abs-day abs-day
         :rel-dc-day rel-dc-day
         :crop crop
         :crop-id (:crop/id crop)
         :donation (donations-at donations abs-day)
         :profit-per-dt (when crop-instance
                          (:crop.instance/avg-expected-profit-per-dt crop-instance))
         :avg-additional-yield-per-mm (when crop-instance
                                        (:crop.instance/avg-expected-additional-yield-per-mm crop-instance))
         :technology-type technology-type
         :technology-outlet-height (or (-> plot :plot.annual/technology :technology/outlet-height) 200)
         :technology-sprinkle-loss-factor (or (-> plot :plot.annual/technology :technology/sprinkle-loss-factor)
                                              (if (= technology-type :technology.type/drip)
                                                0
                                                0.2))
         :tavg (some-> weather :weather-data/average-temperature (bu/round ,,, :digits 1))
         :globrad (some-> weather :weather-data/global-radiation (bu/round ,,, :digits 1))
         :evaporation (bu/round (:weather-data/evaporation weather) :digits 1)
         :precipitation (bu/round (:weather-data/precipitation weather) :digits 1)
         :cover-degree cover-degree
         :qu-target (bu/round
                     (if (< prev-day-cover-degree 1/100)
                       0
                       (interpolated-value (:crop/rel-dc-day-to-quotient-aet-pets crop) rel-dc-day))
                     :digits 3)
         :extraction-depth-cm extraction-depth-cm
         :transpiration-factor (interpolated-value (:crop/rel-dc-day-to-transpiration-factors crop) rel-dc-day)
         :fcs (:plot/field-capacities plot)
         :pwps (:plot/permanent-wilting-points plot)
         :lambdas (or (:lambdas plot) (lambda (:lambda-without-correction plot) abs-day))
         ;database values allow capillary rise rates only up to 2.7m from groundwater table to rooting zone
         :capillary-rise-rates (when (and groundwater-level-cm
                                          (<= (- groundwater-level-cm extraction-depth-cm) 270))
                                 (capillary-rise-rates (:plot/ka5-soil-types plot) extraction-depth-cm))
         :groundwater-level-cm groundwater-level-cm
         :damage-compaction-depth-cm (resulting-damage-compaction-depth-cm plot)
         :soil-moisture-prognosis? false})))))

(defn create-input-seq
  "create the input sequence for all the other functions"
  [plot sorted-weather-map until-abs-day donations technology-type]
  (->> (base-input-seq plot
                       sorted-weather-map
                       donations
                       technology-type)
       (drop-while #(< (:abs-day %) (:plot.annual/abs-day-of-initial-soil-moisture-measurement plot)) ,,,)
       (take-while #(<= (:abs-day %) until-abs-day) ,,,)))

(defn calc-soil-moistures*
  "calculate the soil-moistures for the given inputs and initial soil-moisture returning
  all intermediate steps, unless red-fn is defined to be reduce"
  [inputs initial-soil-moistures & {red-fn :red-fn :or {red-fn reductions}}]
  (red-fn calc-soil-moisture
          {:qu-sum-deficits 0
           :qu-sum-targets 0
           :soil-moistures initial-soil-moistures}
          inputs))

(defn calc-soil-moistures
  "calculate the soil-moistures for the given inputs and initial soil-moisture"
  [inputs initial-soil-moistures]
  (calc-soil-moistures* inputs initial-soil-moistures :red-fn reduce))

(defn average-prognosis-result
  "averages the results returned from calc-soil-moisture for prognosis calculation"
  [no-of-days {:keys [qu-sum-deficits qu-sum-targets] :as m}]
  (assoc m :qu-avg-current (/ (- qu-sum-targets qu-sum-deficits)
                              no-of-days)
           :qu-avg-target (/ qu-sum-targets
                             no-of-days)))

(defn- calc-soil-moisture-prognosis**
  "the common part of the prognosis calculations, no matter if done by reduce or reductions"
  [prognosis-days inputs soil-moistures & {red-fn :red-fn :or {red-fn reductions}}]
  (->> inputs
       ;if we got more input, take just the prognosis days
       (take prognosis-days ,,,)
       ;turn on prognosis mode
       (map #(assoc % :soil-moisture-prognosis? true) ,,,)
       ;calc soil-moistures (either with intermediate values or just last result)
       (red-fn calc-soil-moisture
               {:qu-sum-deficits 0
                :qu-sum-targets 0
                :soil-moistures soil-moistures}
               ,,,)))

(defn calc-soil-moisture-prognosis*
  "calculate the soil-moisture prognosis but returning a list of intermediate results"
  [prognosis-days inputs soil-moistures]
  (->> (calc-soil-moisture-prognosis** prognosis-days inputs soil-moistures :red-fn reductions)
       ;drop first initial value from the reductions function
       rest
       ;calculate averages of result(s)
       (map-indexed (fn [i v] (average-prognosis-result (inc i) v)) ,,,)))

(defn calc-soil-moisture-prognosis
  "calculate the soil-moisture prognosis in prognosis-days using inputs and the given soil-moisture"
  [prognosis-days inputs soil-moistures]
  (->> (calc-soil-moisture-prognosis** prognosis-days inputs soil-moistures :red-fn reduce)
       ;average the returned single result
       (average-prognosis-result prognosis-days ,,,)))

(def recommendation-actions #{:dont-irrigate :irrigate :check-again})

(def recommendation-states
  {:outside-of-irrigation-period {:action :dont-irrigate
                                  :text {:lang/de "Außerhalb des Bewässerungs-/Beregnungszeitraums" #_"Entw/Zeitr"
                                         :lang/en "Outside of irrigation period"}}
   :check-back-soon {:action :check-again
                     :no-of-days 4
                     :text {:lang/de "in ca. 4 Tagen"
                            :lang/en "in about 4 days"}}
   :increase-technological-max-donation {:action :irrigate
                                         :text {:lang/de "Schlagkraft erhöhen" #_"S.K. erh."
                                                :lang/en "increase technological maximal donation"}}
   :optimal-donation {:action :irrigate
                      :text {:lang/de "Gabe optimal" #_"Gabe opt."
                             :lang/en "Donation optimal"}}
   :optimal-soil-moisture {:action :dont-irrigate
                           :text {:lang/de "Bodenfeuchte optimal" #_"Bf opt."
                                  :lang/en "Soil moisture optimal"}}
   :high-soil-moisture {:action :dont-irrigate
                        :text {:lang/de "Bodenfeuchte hoch" #_"Bf hoch"
                               :lang/en "Soil moisture high"}}
   :below-technological-minimum {:action :dont-irrigate
                                 :text {:lang/de "Technologisches minimum" #_"Tech.min"
                                        :lang/en "Technological minimum"}}})

(defn calc-donation
  "calculate the irrigation amount to be given and the according recommendation text"
  [qu-prognosis-target
   {:keys [min-boundary-donation opt-boundary-donation max-boundary-donation
           step-size cycle-days]}
   inputs soil-moistures]
  (let [input (first inputs)

        ;prevent infinite loop at calculating the optimal donation
        step-size (max 1 step-size)

        ;look that many days into the future
        ;should be as many days as cycle days to the technological requirements
        ;but at least 4 days to reflect a valid prognosis time frame
        ;(general prognosis time frame is 5 days)
        irrigation-prognosis-days (max 4 (min cycle-days 14))

        ;a reduced quotient depending on crop specifics to have a lower border
        ;because water stress doesn't impact evey crop the same at the
        ;standard (per crop) target quotient
        qu-eff (- qu-prognosis-target (-> input :crop :crop/effectivity-quotient))

        ;function to calculate the soil-moistures by applying donation amount x at the first day
        ;of a period of length irrigation-prognosis-days
        calc-sms-with-donation (fn [donation]
                                 (->> inputs
                                      (take irrigation-prognosis-days ,,,)
                                      ;set irrigation amount just for first day (of all the irrigation days)
                                      ((fn [[f-input & rest-input]] (cons (assoc f-input :donation donation) rest-input)) ,,,)
                                      ;set sm-prognosis true for all inputs
                                      (map #(assoc % :soil-moisture-prognosis? true) ,,,)
                                      ;calculate soil-moisture for the irrigation days
                                      (reduce calc-soil-moisture
                                              {:qu-sum-deficits 0
                                               :qu-sum-targets 0
                                               :soil-moistures soil-moistures}
                                              ,,,)
                                      ;get the average of the irrigation days
                                      (average-prognosis-result irrigation-prognosis-days ,,,)))

        ;calculate soil-moisture in given future time without any irrigation as base value
        {qu-0-current :qu-avg-current
         qu-0-target :qu-avg-target
         :as qu-avg} (calc-sms-with-donation 0)

        ;_ (println "irr-days: " irrigation-prognosis-days " qu-eff: " qu-eff " qu-0-current: " qu-0-current " qu-0-target: " qu-0-target)
        ]
    (if (< qu-0-current qu-0-target)
      ;without irrigation we're below the target curve
      (if (< qu-eff qu-0-current)
        ;but we're above the effective curve, thus try again in about 4 days
        {:state :check-back-soon}
        ;nope, we've got to irrigate
        (loop [donation opt-boundary-donation
               direction :none]
          (let [{qu-current :qu-avg-current
                 qu-target :qu-avg-target
                 :as qu-avg} (calc-sms-with-donation donation)
                qu-target-high (/ (+ 1 qu-target) 2) #_(/ (+ qu-eff qu-target) 2)
                qu-target-low qu-eff]
            #_(println "direction: " direction " donation: " donation
                     " qu-avg: " (pr-str qu-avg)
                     " qu-target-low: " qu-target-low " qu-target-high: " qu-target-high)
            (if (> qu-current qu-target-high)               ;above qu-target-high -> potential reduction
              (if (or (= direction :up)                     ;either we're moving up from below, thus just crossed (with step-size) qu-target-high
                      (= donation min-boundary-donation))   ;or we're even with minimal donation already above qu-target-high
                {:donation donation
                 :state :optimal-donation}
                ;move down by step-size
                (recur (max (- donation step-size) min-boundary-donation)
                       :down))
              (cond                                         ;below qu-target-hight -> potential increase
                (or (> qu-current qu-target-low)            ;we're within the allowed band
                    (= direction :down))                    ;we're below qu-target-high, but moving :down from above, thus at max we're step-size below qu-target-high
                {:donation donation
                 :state :optimal-donation}

                (= donation max-boundary-donation)          ;we're below qu-target-high, but at maximal possible donation
                {:donation donation
                 :state :increase-technological-max-donation}

                :else
                ;move up by step size
                (recur (min max-boundary-donation (+ donation step-size))
                       :up))))))
      ;without irrigation we're above target curve, thus everything is fine
      {:state :optimal-soil-moisture})))

(defn calc-donation-boundaries
  "calculate the irrigation-water donation data given a soil-moisture and
  the technological restrictions"
  [forecast-days slope {opt-donation-technology :donation/opt
                        donation-step-size :donation/step-size
                        max-donation-technology :donation/max
                        min-donation-technology :donation/min}
   inputs soil-moistures]
  (let [{:keys [abs-day fcs pwps]} (first inputs)

        [max-donation-soil-30
         max-donation-soil-60] (->> (map vector (layer-depths) fcs pwps soil-moistures)
                                    ;create ["depths equal and below 30cm" , "rest"]
                                    (split-with #(<= (first %) 30) ,,,)
                                    (ajuxt first
                                          (rcomp second
                                               ;finally get 30cm < depth <= 60cm
                                               (partial take-while #(<= (first %) 60)))
                                          ,,,)
                                    ;calc difference of inf-barrier and soil-moisture
                                    ;and sum up layers
                                    ;for both parts
                                    (map #(->> %
                                               (reduce (fn [sum [depth-cm fc pwp sm]]
                                                         (+ sum
                                                            (- (infiltration-barrier fc pwp abs-day depth-cm) sm)))
                                                       0 ,,,)
                                               double
                                               Math/round)
                                         ,,,))

        ;it's a little bit unclear, if that is supposed to check whether
        ;just soil-60 is negative and thus don't add it,
        ;but on the other side if soil-30 and -60 are negative, it will
        ;be added as well
        ;the question is, if that can happen at all, actually only if the
        ;infiltration barrier can get smaller than the soil-moisture in the layer
        ;for now I keep it that way, but should be fixed
        max-donation-soil+60 (+ max-donation-soil-30
                                (if (pos? (* max-donation-soil-30 max-donation-soil-60))
                                  max-donation-soil-60
                                  0))

        max-donation-soil+60+weather (->> inputs
                                          (reduce (fn [sum {:keys [evaporation precipitation]}]
                                                    (+ sum (- evaporation precipitation)))
                                                  max-donation-soil+60
                                                  ,,,)
                                          double
                                          Math/round
                                          (max 0. ,,,))

        max-donation-slope (condp #(% %2) slope
                             #{1 2} 50
                             #{3} 40
                             #{4 5} 30
                             #{6} 20)

        max-possible-donation (min max-donation-technology max-donation-slope max-donation-soil+60+weather)

        ;get lowest maximal donation supported by technology
        max-possible-and-supported-donation (* donation-step-size (quot max-possible-donation donation-step-size))

        opt-donation (if (< max-possible-and-supported-donation opt-donation-technology)
                       (->> opt-donation-technology
                            (iterate #(- % donation-step-size) ,,,)
                            (drop-while #(< max-possible-and-supported-donation %) ,,,)
                            first)
                       opt-donation-technology)

        ;_ (debug [max-donation-technology max-donation-slope max-donation-soil+60+weather max-possible-and-supported-donation opt-donation])

        high-soil-moisture? (< max-donation-soil+60+weather 15)]

    {:high-soil-moisture? high-soil-moisture?
     :min-boundary-donation (if high-soil-moisture? 0 min-donation-technology)
     :opt-boundary-donation opt-donation
     :max-boundary-donation max-possible-and-supported-donation}))

(defn calc-recommendation
  "calculate the recommendation text and recommendation donation amount for the given input values"
  [prognosis-days slope technology inputs soil-moistures]
  (let [;get the boundaries depending on current soil-moisture, used technology and slope
        {:keys [high-soil-moisture?
                max-boundary-donation
                min-boundary-donation
                opt-boundary-donation]
         :as boundaries} (calc-donation-boundaries prognosis-days slope
                                                   technology inputs soil-moistures)

        ;_ (println "prognosis-days: " prognosis-days " boundaries: " (pr-str boundaries))

        technology+boundaries (-> boundaries
                                  (dissoc ,,, :high-soil-moisture?)
                                  (assoc ,,, :cycle-days (:technology/cycle-days technology)
                                             :step-size (:donation/step-size technology)))

        {qu-prognosis-avg-current :qu-avg-current
         qu-prognosis-avg-target :qu-avg-target}
        (calc-soil-moisture-prognosis prognosis-days inputs soil-moistures)

        ;_ (println "--> qu-prognosis-avg-current: " qu-prognosis-avg-current " qu-prognosis-avg-current: " qu-prognosis-avg-target)
        ]
    (cond
     (< qu-prognosis-avg-current qu-prognosis-avg-target)
     (cond

      high-soil-moisture?
      {:state :high-soil-moisture}

      (>= max-boundary-donation min-boundary-donation)
      (do
        #_(println "qu-prognosis-avg-current: " qu-prognosis-avg-current " < qu-prognosis-avg-target: " qu-prognosis-avg-target )
        #_(println "technology+boundaries: " (pr-str technology+boundaries))
        (calc-donation qu-prognosis-avg-target technology+boundaries inputs soil-moistures))

      :else
      {:state :below-technological-minimum})

     (< qu-prognosis-avg-target 1/10)
     {:state :outside-of-irrigation-period}

     :else
     {:state :optimal-soil-moisture})))

(defn calculate-soil-moistures-by-auto-donations**
  "calculate soil-moistures but apply automatically the recommended donations"
  [inputs initial-soil-moistures slope technology prognosis-days
   & {red-fn :red-fn :or {red-fn reduce}}]
  (red-fn (fn [{:keys [days-to-go current-sm sum-donations]} [f-input :as prognosis-inputs]]
            (let [{:keys [days-to-go* donation]}
                  (if (zero? days-to-go)
                    (let [
                          ;_ (println "abs-day: " (:abs-day f-input))
                          {:keys [state donation] :as rec}
                          (calc-recommendation (count prognosis-inputs) slope technology
                                               prognosis-inputs (:soil-moistures current-sm))
                          ;_ (println "state: " state " recommended donation: " donation)
                          ]
                      (case state
                        (:optimal-soil-moisture
                         :outside-of-irrigation-period
                         :below-technological-minimum
                         :high-soil-moisture) {:days-to-go* 1
                                               :donation 0}

                        (:check-back-soon) {:days-to-go* (-> recommendation-states
                                                             :check-back-soon
                                                             :no-of-days
                                                             #_inc)
                                            :donation 0}

                        (:increase-technological-max-donation
                         :optimal-donation) {:days-to-go* (:technology/cycle-days technology) #_prognosis-days
                                             :donation donation}))
                    {:days-to-go* days-to-go
                     :donation 0})

                  ;_ (println "days-to-go*: " (dec days-to-go*) " final donation to apply: " donation)
                  ]
              #_(println "abs-day: " (:abs-day f-input)
                       " crop-id: " (:crop-id f-input)
                       " dc: " (:dc f-input)
                       " evap: " (:evaporation f-input)
                       " precip: " (:precipitation f-input)
                       " extraction-depth-cm: " (:extraction-depth-cm f-input)
                       " cover-degree: " (:cover-degree f-input)
                       " prev-sm: " (str (:soil-moistures current-sm)))
              {:days-to-go (dec days-to-go*)
               :sum-donations (+ sum-donations donation)
               :current-sm (calc-soil-moisture current-sm (assoc f-input :donation donation))}))
          {:days-to-go 0
           :sum-donations 0
           :current-sm {:qu-sum-deficits 0
                        :qu-sum-targets 0
                        :soil-moistures initial-soil-moistures}}
          (partition-all prognosis-days 1 inputs)))

(defn calculate-soil-moistures-by-auto-donations
  [inputs initial-soil-moistures slope technology prognosis-days]
  (->> (calculate-soil-moistures-by-auto-donations** inputs initial-soil-moistures slope technology
                                                     prognosis-days :red-fn reduce)
       :current-sm))

(defn calculate-sum-donations-by-auto-donations
  [inputs initial-soil-moistures slope technology prognosis-days]
  (->> (calculate-soil-moistures-by-auto-donations** inputs initial-soil-moistures slope technology
                                                     prognosis-days :red-fn reduce)
       :sum-donations))

(defn calculate-soil-moistures-by-auto-donations*
  "calculate soil-moistures but apply automatically the recommended donations"
  [inputs initial-soil-moistures slope technology prognosis-days]
  (->> (calculate-soil-moistures-by-auto-donations** inputs initial-soil-moistures slope technology
                                                     prognosis-days :red-fn reductions)
       rest
       (map :current-sm ,,,)))

(defn calc-soil-moistures*
  "calculate the soil-moistures for the given inputs and initial soil-moisture returning
  all intermediate steps, unless red-fn is defined to be reduce"
  [inputs initial-soil-moistures & {red-fn :red-fn :or {red-fn reductions}}]
  (-> (red-fn calc-soil-moisture
              {:qu-sum-deficits 0
               :qu-sum-targets 0
               :soil-moistures initial-soil-moistures}
              inputs)
      rest))

(defn calc-soil-moistures
  "calculate the soil-moistures for the given inputs and initial soil-moisture"
  [inputs initial-soil-moistures]
  (calc-soil-moistures* inputs initial-soil-moistures :red-fn reduce))

