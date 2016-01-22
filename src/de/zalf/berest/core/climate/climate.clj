(ns de.zalf.berest.core.climate.climate
  (:import (java.util Date))
  (:require [clojure.java.io :as cjio]
            [clojure.string :as str]
            [clj-time.core :as ctc]
            [clj-time.format :as ctf]
            [clj-time.coerce :as ctcoe]
            [de.zalf.berest.core.climate.algo :as algo]
            [de.zalf.berest.core.datomic :as db]
            [de.zalf.berest.core.util :as bu]
            [de.zalf.berest.core.queries :as queries]
            [datomic.api :as d]
            [clojure.pprint :as pp])
  (:import java.util.Date))

(defn- before-start+after-end-of-year
  [year]
  [(ctcoe/to-date (ctc/date-time (dec year) 12 31))
   (ctcoe/to-date (ctc/date-time (inc year) 1 1))])

(defn- weather-data-in-year?
  [start-of-year-1 end-of-year+1 {date :weather-data/date} ]
  (and (.after date start-of-year-1)
       (.before date end-of-year+1)))

(defn weather-station-data
  [db year weather-station-id]
  (let [[start-of-year-1 end-of-year+1] (before-start+after-end-of-year year)
        data (d/q '[:find ?ws-e-id ?data-e-id
                    :in $ ?ws-id ?soy-1 ?eoy+1
                    :where
                    [?ws-e-id :weather-station/id ?ws-id]
                    [?ws-e-id :weather-station/data ?data-e-id]
                    [?data-e-id :weather-data/date ?date]
                    [(.after ?date ?soy-1)]
                    [(.before ?date ?eoy+1)]]
                  db weather-station-id start-of-year-1 end-of-year+1)]
    {:station (d/entity db (ffirst data))
     :data (map #(d/entity db (second %)) data)}))

(declare longterm-evap-precip)

(defn final-sorted-weather-data-map-for-plot
  [db year measured-doy prognosis-days plot-id]
  (let [plot-e (first (db/query-entities db :plot/id plot-id))
        {plot-wstation-e :plot/weather-station
         plot-wdata-es :plot/weather-data
         farm-e :farm/_plots} plot-e
        {auth-farm-wstation-e :farm/authorative-weather-station
         farm-wstation-e :farm/weather-station
         farm-wdata-es :farm/weather-data} farm-e

        [start-of-year-1 end-of-year+1] (before-start+after-end-of-year year)

        wdiy (partial weather-data-in-year? start-of-year-1 end-of-year+1)

        doy-2-data-seq (fn [weather-data-seq]
                         (map #(vector (bu/date-to-doy (:weather-data/date %)) (d/touch %))
                              weather-data-seq))

        filter-measured-data (fn [measured-doy doy-2-data-seq]
                               (filter (fn [[doy v]] (and (not (:weather-data/prognosis-date v))
                                                          (<= doy (+ measured-doy prognosis-days) #_measured-doy)))
                                       doy-2-data-seq))

        filter-prognosis-data (fn [measured-doy doy-2-data-seq]
                                (filter (fn [[doy v]] (and (:weather-data/prognosis-date v)
                                                           (= (bu/date-to-doy (:weather-data/prognosis-date v))
                                                              (inc measured-doy))
                                                           (< measured-doy doy (+ measured-doy prognosis-days 1))))
                                        doy-2-data-seq))

        data-as-sorted-map* (fn [measured-doy weather-data-result]
                              (some->> weather-data-result
                                       :data
                                       doy-2-data-seq
                                       ((juxt (partial filter-prognosis-data measured-doy)
                                              (partial filter-measured-data measured-doy)) ,,,)
                                       (map #(into (sorted-map) %),,,)))
        data-as-sorted-map (partial data-as-sorted-map* measured-doy)

        get-ws-data (fn [ws-e]
                      (some->> ws-e
                               :weather-station/id
                               (weather-station-data db year ,,,)
                               data-as-sorted-map)
                      #_(let [all-weather-data-from-year (some->> ws-e
                                                                :weather-station/id
                                                                (weather-station-data db year ,,,)
                                                                data-as-sorted-map)
                            [prognosis-data measured-data] (some->> all-weather-data-from-year
                                                                    )

                            ;available-prog-days (count prognosis-data)
                            #_prognosis-data*
                            #_(if (< available-prog-days prognosis-days)
                              (let [more-measured-data (some->> all-weather-data-from-year
                                                                :data
                                                                doy-2-data-seq
                                                                ;get extended measured data (if possible) for prognosis time
                                                                (filter-measured-data (+ measured-doy prognosis-days) ,,,)
                                                                ;filter out all measured days
                                                                (remove (fn [[doy v]] (<= doy measured-doy)) ,,,)
                                                                (into (sorted-map) ,,,))
                                    ;overwrite possibly existing prognosis-data with measured data
                                    p-data* (merge more-measured-data prognosis-data)

                                    ;still missing prognosis data? -> extend with longterm averages
                                    available-prog-days* (count p-data*)
                                    p-data** (when (< available-prog-days* prognosis-days)
                                                       (merge (into (sorted-map)
                                                                    (for [i (range available-prog-days* prognosis-days)]
                                                                      (let [doy (+ measured-doy i)]
                                                                        [doy (longterm-evap-precip doy)])))
                                                              p-data*))]
                                p-data**)
                              prognosis-data)]
                        [prognosis-data measured-data]))

        #_get-ws-data #_(fn [ws-e]
                      (some->> ws-e
                               :weather-station/id
                               (weather-station-data db year ,,,)
                               data-as-sorted-map))

        ;_ (println "measured-doy: " measured-doy " prognosis-days: " prognosis-days)

        [auth-farm-ws-data-p auth-farm-ws-data] (get-ws-data auth-farm-wstation-e)
        ;_ (println "auth-farm-ws-data: " (type auth-farm-ws-data) " " (pr-str auth-farm-ws-data))

        [farm-ws-data-p farm-ws-data] (get-ws-data farm-wstation-e)
        ;_ (println "farm-ws-data: " (type farm-ws-data) " " (pr-str farm-ws-data))

        [farm-wdata-p farm-wdata] (data-as-sorted-map {:data (filter wdiy farm-wdata-es)})
        ;_ (println "farm-wdata: " (type farm-wdata) " " (pr-str farm-wdata))

        [plot-ws-data-p plot-ws-data] (get-ws-data plot-wstation-e)
        ;_ (println "plot-ws-data: " (type plot-ws-data) " " (pr-str plot-ws-data))

        [plot-wdata-p plot-wdata] (data-as-sorted-map {:data (filter wdiy plot-wdata-es)})
        ;_ (println "plot-wdata: " (type plot-wdata) " " (pr-str plot-wdata))

        longterm-data (into (sorted-map)
                            (for [i (range prognosis-days)]
                              (let [doy (+ measured-doy i 1)
                                    data (longterm-evap-precip doy)]
                                (when data
                                  [doy data]))))
        ;_ (println "longterm-data: " (type longterm-data) " " (pr-str longterm-data))

        #__ #_(println "merge-with: " (pr-str (merge-with #(merge (into {} %1) (into {} %2))
                                                      (sorted-map)
                                                      longterm-data
                                                      auth-farm-ws-data auth-farm-ws-data-p
                                                      farm-ws-data farm-ws-data-p
                                                      farm-wdata farm-wdata-p
                                                      plot-ws-data plot-ws-data-p
                                                      plot-wdata plot-wdata-p)))
        ]
    ; merge all days, more specific data (like plot-data) overwriting more general ones (on a doy basis)
    ; if two or more days are the same, then merge the elements again, thus more specific elements like
    ; an extreme precipitation event will overwrite an more general precipitation event from the authorative station etc
    ; -> due to restrictions in data-as-sorted-map prognosis values should never overlap, thus overwrite, measured data
    ; ... the ugly (merge (into {} %1) ... etc. is necessary, because all the data are still datomic.query.EntityMaps
    ; and thus can't be used with merge because this aren't really complete maps
    (merge-with #(merge (into {} %1) (into {} %2))
                (sorted-map)
                longterm-data
                auth-farm-ws-data auth-farm-ws-data-p
                farm-ws-data farm-ws-data-p
                farm-wdata farm-wdata-p
                plot-ws-data plot-ws-data-p
                plot-wdata plot-wdata-p)))

(defn sorted-weather-data-map
  [db weather-station-id year]
  (->> (weather-station-data db weather-station-id year)
       (map #(vector (bu/date-to-doy (:weather-data/date %)) %) ,,,)
       (into (sorted-map) ,,,)))

(comment "retract some stations climate data"

  (d/transact (db/connection) [[:db.fn/retractEntity 17592186062454]
                               [:db.fn/retractEntity 17592186067570]])

  )

(comment "find some stations"

  (def db (db/current-db "berest"))
  (def con (db/connection "berest"))
  (def stations (queries/get-entities db :weather-station/id))
  (filter #(.startsWith (:weather-station/id %) "zalf/") stations)

  (d/transact con [{:db/id [:weather-station/id "de.zalf/zalf"]
                    :weather-station/id "zalf/zalf"}])

  )


(comment "check weather-data of a station"

  (def t (weather-station-data (db/current-db "berest") "zalf/zalf" 1993))
  (sort-by :weather-data/date (map d/touch (:data t)))
  (count (:data t))

  )


(defn longterm-evap-precip [doy]
  (let [longterm-average-evaporation-values
        [#_"01.04." 1.1, 1.2, 1.2, 1.2, 1.3, 1.3, 1.3, 1.4, 1.4, 1.4,
         #_"11.04." 1.4, 1.5, 1.5, 1.5, 1.6, 1.6, 1.6, 1.7, 1.7, 1.7,
         #_"21.04." 1.8, 1.8, 1.8, 1.9, 1.9, 1.9, 2.0, 2.0, 2.0, 2.1,
         #_"01.05." 2.1, 2.1, 2.2, 2.2, 2.2, 2.3, 2.3, 2.3, 2.4, 2.4,
         #_"11.05." 2.4, 2.5, 2.5, 2.5, 2.6, 2.6, 2.6, 2.7, 2.7, 2.7,
         #_"21.05." 2.7, 2.8, 2.8, 2.8, 2.9, 2.9, 2.9, 2.9, 3.0, 3.0, 3.0,
         #_"01.06." 3.0, 3.1, 3.1, 3.1, 3.2, 3.2, 3.2, 3.2, 3.3, 3.3,
         #_"11.06." 3.3, 3.4, 3.4, 3.4, 3.4, 3.4, 3.4, 3.4, 3.4, 3.4,
         #_"21.06." 3.4, 3.4, 3.4, 3.4, 3.4, 3.4, 3.4, 3.4, 3.4, 3.4,
         #_"01.07." 3.4, 3.4, 3.3, 3.3, 3.3, 3.3, 3.3, 3.3, 3.3, 3.3,
         #_"11.07." 3.3, 3.3, 3.3, 3.3, 3.3, 3.3, 3.3, 3.2, 3.2, 3.2,
         #_"21.07." 3.2, 3.2, 3.2, 3.1, 3.1, 3.1, 3.1, 3.1, 3.1, 3.1, 3.0,
         #_"01.08." 3.0, 3.0, 3.0, 3.0, 3.0, 2.9, 2.9, 2.9, 2.9, 2.9,
         #_"11.08." 2.9, 2.9, 2.8, 2.8, 2.8, 2.8, 2.7, 2.7, 2.7, 2.7,
         #_"21.08." 2.6, 2.6, 2.6, 2.6, 2.5, 2.5, 2.4, 2.4, 2.4, 2.4, 2.4,
         #_"01.09." 2.3, 2.3, 2.3, 2.2, 2.2, 2.2, 2.2, 2.1, 2.1, 2.1,
         #_"11.09." 2.0, 2.0, 2.0, 2.0, 1.9, 1.9, 1.9, 1.8, 1.8, 1.8,
         #_"21.09." 1.7, 1.7, 1.6, 1.6, 1.6, 1.5, 1.5, 1.5, 1.4, 1.4,
         #_"01.10." 1.4, 1.3, 1.3, 1.3, 1.2, 1.2, 1.2, 1.1, 1.1, 1.0,
         #_"11.10." 1.0, 1.0, 0.9, 0.9, 0.9, 0.7, 0.6, 0.7, 0.5, 0.8,
         #_"21.10." 1.0, 1.0, 0.9, 0.9, 0.9, 0.7, 0.6, 0.7, 0.5, 0.8]
        longterm-average-precipitation-values
        [#_"01.04." 1.0, 1.7, 0.6, 0.5, 1.1, 0.9, 0.9, 0.9, 1.9, 1.5,
         #_"11.04." 1.1, 0.8, 1.2, 1.5, 2.2, 0.9, 1.4, 1.1, 2.0, 0.9,
         #_"21.04." 0.7, 1.3, 0.9, 0.9, 0.4, 0.6, 0.9, 1.0, 2.0, 1.6,
         #_"01.05." 1.3, 1.1, 2.0, 1.5, 1.6, 1.8, 1.9, 1.3, 1.0, 1.3,
         #_"11.05." 4.2, 0.6, 1.6, 1.5, 1.3, 0.6, 0.9, 1.9, 1.4, 4.6,
         #_"21.05." 1.0, 0.9, 0.4, 0.9, 2.7, 1.0, 3.6, 2.8, 0.7, 2.2, 2.3,
         #_"01.06." 1.1, 2.2, 0.6, 1.3, 1.0, 0.8, 0.7, 2.7, 4.4, 3.5,
         #_"11.06." 2.0, 6.0, 1.3, 1.0, 1.8, 1.9, 1.5, 1.0, 3.3, 1.5,
         #_"21.06." 1.9, 2.8, 0.7, 0.6, 3.6, 2.4, 4.1, 3.3, 3.5, 1.9,
         #_"01.07." 1.6, 1.5, 1.9, 3.0, 3.4, 1.9, 1.1, 0.9, 2.5, 1.2,
         #_"11.07." 1.3, 2.2, 1.5, 1.0, 2.5, 2.0, 1.9, 3.4, 1.1, 4.3,
         #_"21.07." 3.6, 3.7, 3.6, 1.5, 0.9, 1.4, 2.1, 1.0, 1.4, 1.2, 0.9,
         #_"01.08." 0.8, 0.5, 2.4, 1.7, 1.0, 1.3, 0.8, 1.7, 1.9, 1.3,
         #_"11.08." 2.0, 1.7, 1.4, 0.3, 2.3, 1.7, 2.8, 1.1, 1.1, 3.1,
         #_"21.08." 1.6, 2.9, 1.2, 1.4, 2.6, 1.4, 2.4, 3.2, 4.0, 1.6, 0.6,
         #_"01.09." 2.1, 0.5, 0.3, 1.0, 1.4, 1.6, 3.1, 1.8, 2.6, 2.3,
         #_"11.09." 2.9, 1.0, 1.2, 1.9, 0.6, 2.0, 1.8, 1.1, 0.7, 1.2,
         #_"21.09." 0.6, 1.5, 0.6, 2.3, 1.2, 0.9, 0.6, 2.2, 2.3, 1.0,
         #_"01.10." 0.6, 0.8, 2.5, 0.4, 0.7, 0.5, 2.1, 0.5, 1.1, 2.4,
         #_"11.10." 0.8, 0.2, 0.9, 1.6, 1.0, 2.5, 1.7, 1.6, 1.5, 1.0,
         #_"21.10." 2.2, 2.9, 1.8, 1.4, 1.2, 0.6, 1.3, 2.0, 0.4, 1.9]]
    (when (and (< 90 doy) (<= doy (+ 90 213)))
      (let [index (- doy 90 1)]
        {:weather-data/evaporation (nth longterm-average-evaporation-values index),
         :weather-data/precipitation (nth longterm-average-precipitation-values index)}))))

(defn weather-at [m doy]
  (if-let [v (find m doy)]
    (second v)
    (assoc (longterm-evap-precip doy) :prognosis? true)))



