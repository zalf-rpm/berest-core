(ns de.zalf.berest.core.api
  (:require [clojure.string :as cs]
            [datomic.api :as d]
            [clojure.pprint :as pp]
            [clojure.edn :as edn]
            [clojure.tools.logging :as log]
            [de.zalf.berest.core.core :as bc]
            #_[de.zalf.berest.core.plot :as plot]
            [de.zalf.berest.core.datomic :as db]
            [de.zalf.berest.core.util :as bu]
            [de.zalf.berest.core.helper :as bh :refer [rcomp]]
            [de.zalf.berest.core.climate.climate :as climate]
            [simple-time.core :as time]
            [clj-time.core :as ctc]
            [clj-time.format :as ctf]
            [clj-time.coerce :as ctcoe]
            [clojure-csv.core :as csv]))

(defn create-csv-output
  [inputs full-reductions-results]
  (let [value->german-string
        (fn [value]
          (if (nil? value)
            ""
            (if (instance? String value)
              value
              (.. java.text.NumberFormat (getInstance java.util.Locale/GERMAN) (format value)))))

        header-line ["doy"
                     "date"
                     "rel DC day"
                     "precip [mm]"
                     "tavg [°]"
                     "globrad [Jpcm2]"
                     "evap [mm]"
                     "irrWater [mm]"
                     "pet [mm]"
                     "aet [mm]"
                     "aet/pet"
                     "aet/pet soll"
                     "infil 200cm [mm]"
                     "sm 0-30cm [mm]"
                     "sm 30-60cm [mm]"
                     "sm 60-90cm [mm]"
                     "sm 5cm [mm]"
                     "sm 10cm [mm]"
                     "sm 20cm [mm]"
                     "sm 30cm [mm]"
                     "sm 40cm [mm]"
                     "sm 50cm [mm]"
                     "sm 60cm [mm]"
                     "sm 70cm [mm]"
                     "sm 80cm [mm]"
                     "sm 90cm [mm]"
                     "sm 100cm [mm]"
                     "sm 110cm [mm]"
                     "sm 120cm [mm]"
                     "sm 130cm [mm]"
                     "sm 140cm [mm]"
                     "sm 150cm [mm]"
                     "sm 160cm [mm]"
                     "sm 170cm [mm]"
                     "sm 180cm [mm]"
                     "sm 190cm [mm]"
                     "sm 200cm [mm]"
                     "sm 0-10cm [mm]"
                     "sm 10-20cm [mm]"
                     "sm 30-60cm [mm]"
                     "sm 60-100cm [mm]"
                     "sm 100-150cm [mm]"
                     "effective-precipitation [mm]"
                     "effective-donation [mm]"
                     "effective-donation-uncovered [mm]"
                     "cover-degree [%]"
                     "dc"
                     "extraction-depth [cm]"]

        body-lines (map (fn [input rres]
                          (map value->german-string
                               [(:abs-day input)
                                (ctf/unparse (ctf/formatter "dd.MM.")
                                             (bu/doy-to-date (:abs-day input)))
                                (:rel-dc-day input)
                                (:precipitation input)
                                (:tavg input)
                                (:globrad input)
                                (- (:evaporation input))
                                (:donation rres #_input)
                                (:pet rres)
                                (:aet rres)
                                (:aet7pet rres)
                                (:qu-target rres)
                                (:groundwater-infiltration rres)
                                (bu/sum (subvec (vec (:soil-moistures rres)) 0 4))
                                (bu/sum (subvec (vec (:soil-moistures rres)) 4 7))
                                (bu/sum (subvec (vec (:soil-moistures rres)) 7 10))
                                (nth (:soil-moistures rres) 0)
                                (nth (:soil-moistures rres) 1)
                                (nth (:soil-moistures rres) 2)
                                (nth (:soil-moistures rres) 3)
                                (nth (:soil-moistures rres) 4)
                                (nth (:soil-moistures rres) 5)
                                (nth (:soil-moistures rres) 6)
                                (nth (:soil-moistures rres) 7)
                                (nth (:soil-moistures rres) 8)
                                (nth (:soil-moistures rres) 9)
                                (nth (:soil-moistures rres) 10)
                                (nth (:soil-moistures rres) 11)
                                (nth (:soil-moistures rres) 12)
                                (nth (:soil-moistures rres) 13)
                                (nth (:soil-moistures rres) 14)
                                (nth (:soil-moistures rres) 15)
                                (nth (:soil-moistures rres) 16)
                                (nth (:soil-moistures rres) 17)
                                (nth (:soil-moistures rres) 18)
                                (nth (:soil-moistures rres) 19)
                                (nth (:soil-moistures rres) 20)
                                (bu/sum (subvec (vec (:soil-moistures rres)) 0 2))
                                (bu/sum (subvec (vec (:soil-moistures rres)) 2 4))
                                (bu/sum (subvec (vec (:soil-moistures rres)) 4 7))
                                (bu/sum (subvec (vec (:soil-moistures rres)) 7 11))
                                (bu/sum (subvec (vec (:soil-moistures rres)) 11 16))
                                (:effective-precipitation rres)
                                (:effective-donation rres)
                                (:effective-donation-uncovered rres)
                                (* (:cover-degree input) 100)
                                (:dc input)
                                (:extraction-depth-cm input)]))
                        inputs full-reductions-results)]
    (cons header-line body-lines)))


(defn calculate-plot-from-db
  [db plot-id plot-annual-id calculation-doy prognosis-days donations dc-assertions]
  (when-let [plot (bc/deep-db->plot db plot-id plot-annual-id)]
    (let [year (:plot.annual/year plot)
          measured-doy (dec calculation-doy)

          ;_ (println "measured-doy: " measured-doy " prognosis-days: " prognosis-days)

          ;plot could be updated with given dc-assertions
          ;plot* (update-in plot [])

          sorted-weather-map (climate/final-sorted-weather-data-map-for-plot db year measured-doy
                                                                             prognosis-days plot-id)
          ;_ (println "sorted-weather-map: " (pr-str (map #(vector (first %) (-> % second :weather-data/precipitation)) sorted-weather-map)))
          ;_ (println "sorted-weather-map: " (type sorted-weather-map))
          ;_ (pp/pprint sorted-weather-map)

          inputs (bc/create-input-seq plot sorted-weather-map (+ measured-doy prognosis-days)
                                      donations :technology.type/sprinkler)
          ;first-doy (-> inputs first :abs-day)
          last-doy (-> inputs last :abs-day)
          available-prognosis-days (inc (- last-doy calculation-doy))
          inputs-x (if (> last-doy calculation-doy)
                     (drop-last available-prognosis-days inputs)
                     inputs)

          ;xxx (map (rcomp (juxt :abs-day :irrigation-amount) str) inputs-x)
          ;_ (println "xxx: " xxx)

          prognosis-inputs (when (> last-doy calculation-doy)
                             (take-last available-prognosis-days inputs))
          ;_ (println "prognosis-inputs: " (pr-str prognosis-inputs))
          ;days (range (-> inputs first :abs-day) (+ calculation-doy prognosis-days 1))

          measured-sms* (bc/calc-soil-moistures* inputs-x (:plot.annual/initial-soil-moistures plot))
          {measured-sms :soil-moistures
           :as sms-x} (last measured-sms*)
          ;(bc/calc-soil-moistures inputs-7 (:plot.annual/initial-soil-moistures plot))

          ;_ (println "measured-sms*: " (take 5 measured-sms*))

          prognosis-sms* (when prognosis-inputs
                           (bc/calc-soil-moisture-prognosis* available-prognosis-days prognosis-inputs measured-sms))
          ;prognosis-sms (last prognosis-sms*)
          #_(bc/calc-soil-moisture-prognosis 7 prognosis-inputs soil-moistures-7)

          ;_ (println "prognosis-sms*: " prognosis-sms*)
          ]
      {:inputs inputs
       :soil-moistures (concat measured-sms* #_(rest measured-sms*) prognosis-sms*)})))


(defn simulate-plot-from-db
  [db plot-id calculation-doy year donations dc-assertions]
  (when-let [plot (bc/deep-db->plot db plot-id year)]
    (let [;plot could be updated with given dc-assertions
          ;plot* (update-in plot [])

           sorted-weather-map (climate/final-sorted-weather-data-map-for-plot db year calculation-doy
                                                                              7 plot-id)

           inputs (bc/create-input-seq plot
                                       sorted-weather-map
                                       (+ calculation-doy 7)
                                       donations
                                       (-> plot :plot.annual/technology :technology/type))

          ;xxx (map (rcomp (juxt :abs-day :irrigation-amount) str) inputs-7)
          ;_ (println xxx)

           days (range (-> inputs first :abs-day) (inc calculation-doy))

           sms* (bc/calculate-soil-moistures-by-auto-donations*
                  inputs (:plot.annual/initial-soil-moistures plot)
                  (-> plot :plot/slope :slope/key)
                  (:plot.annual/technology plot)
                  5)

           {soil-moistures :soil-moistures
            :as sms} (last sms*)
          #_(bc/calculate-soil-moistures-by-auto-donations* inputs (:plot.annual/initial-soil-moistures plot)
                                                          (-> slope :plot/slope :slope/key) (:plot.annual/technology plot) 5)
           ]
      {:inputs inputs
       :soil-moistures sms*})))





#_(defn run [db crop-id plot sorted-weather-map irrigation-donations-map until-abs-day irrigation-type]
  (bc/append-out bc/out str (str (-> plot :plot/number str (subs ,,, 0 3)) "-"
                           (-> plot :plot/number str (subs ,,, 3 4)) " "
                           (-> plot :assertion/dc-assertions first :assertion/crop crop-id) " "
                           (-> plot :assertion/dc-assertions first :assertion/crop :symbol) "      "))

  (let [inputs (bc/create-input-seq plot sorted-weather-map (+ until-abs-day 7)
                                 irrigation-donations-map irrigation-type)
        _ (println "inputs:" \newline "----------------------")
        _ (pp/pprint inputs)
        _ (println "----------------------")

        inputs-7 (drop-last 7 inputs)
        prognosis-inputs (take-last 7 inputs)

        sms-7* (bc/calc-soil-moistures* inputs-7 (:plot.annual/initial-soil-moistures plot))
        _ (println "soil-moistures-7:" \newline "----------------------")
        _ (pp/pprint sms-7*)
        _ (println "----------------------")
        {soil-moistures-7 :soil-moistures
         :as sms-7} (last sms-7*)
        #_(calc-soil-moistures inputs-7 (:plot.annual/initial-soil-moistures plot))

        prognosis* (bc/calc-soil-moisture-prognosis* 7 prognosis-inputs soil-moistures-7)
        _ (println "prognosis:" \newline "----------------------")
        _ (pp/pprint prognosis*)
        _ (println "----------------------")
        prognosis (last prognosis*)
        #_(calc-soil-moisture-prognosis 7 prognosis-inputs soil-moistures-7)

        {:keys [recommendation-text recommended-donation]
         :as rec} (bc/calc-recommendation (-> plot :plot/slope :slope/key) (:plot.annual/technology plot)
                                       prognosis-inputs soil-moistures-7)
        _ (println "recommendation:" \newline "----------------------")
        _ (pp/pprint rec)
        _ (println "----------------------")
        ]
    (spit "out.csv" (csv/write-csv (create-csv-output inputs (concat sms-7* prognosis*))))))







