(ns de.zalf.berest.core.import.zalf-climate-data
  (:require [clojure.java.io :as cjio]
            [clojure.string :as str]
            [clj-time.core :as ctc]
            [clj-time.format :as ctf]
            [clj-time.coerce :as ctcoe]
            [de.zalf.berest.core.climate.algo :as algo]
            [de.zalf.berest.core.datomic :as db]
            [de.zalf.berest.core.util :as bu]
            [datomic.api :as d]
            [clojure.pprint :as pp]
            [clojure.tools.logging :as log]
            [clojure-csv.core :as csv]))

(defn- read-and-parse-as-csv
  [file]
  (-> file
      cjio/resource
      slurp
      (csv/parse-csv :delimiter \,)))

(def climate-data-zalf-1992-1998
  #(read-and-parse-as-csv "private/climate/climate-data-muencheberg-zalf-1992-to-1998.csv"))

(def climate-data-muencheberg-1993-1998
  #(read-and-parse-as-csv "private/climate/climate-data-muencheberg-1993-to-1998.csv"))

(defn muencheberg-csv-data->transaction-weather-data
  [climate-data]
  (map (fn [line]
         (let [[tavg precip globrad] (map #(Double/parseDouble %) (drop 3 line))
               date (ctcoe/to-date ((fn [[d m y]] (str y "-" m "-" d)) (take 3 line)))]
           {:weather-data/date date
            :weather-data/precipitation precip
            :weather-data/evaporation (algo/potential-evaporation-turc-wendling globrad tavg)
            :weather-data/average-temperature tavg
            :weather-data/global-radiation globrad}))
       (rest climate-data)))

(defn transact-data
  [db-connection station-t-data weather-t-data]
  (let [s-t-data (merge {:db/id (db/new-entity-id :climate)} station-t-data)
        t-data (assoc s-t-data :weather-station/data weather-t-data)]
    (try
      #_(println "t-data: " [t-data])
      @(d/transact db-connection [t-data])
      (catch Exception e
        (log/info "Couldn't transact weather data to datomic! data: [\n" t-data "\n]")
        (throw e)))))

(defn transact-zalf-data
  [db-connection]
  (let [station-to-data {{:weather-station/id "zalf_lokal"
                          :weather-station/name "ZALF Wetterstation"
                          :user/_weather-stations [:user/id "zalf"]} (climate-data-zalf-1992-1998)
                         {:weather-station/id "zalf_muencheberg"
                          :weather-station/name "DWD Station Müncheberg"
                          :user/_weather-stations [:user/id "zalf"]} (climate-data-muencheberg-1993-1998)}]
    (->> (vals station-to-data)
         (map muencheberg-csv-data->transaction-weather-data ,,,)
         (map (partial transact-data db-connection) (keys station-to-data) ,,,))))

(defn import-hoplon-client-csv-data
  [db-connection user-id weather-station-id csv-data {:keys [separator decimal-separator ignore-lines
                                                             element-order date-format]}]
  (let [csv (csv/parse-csv csv-data :delimiter (if (= (type separator) java.lang.String)
                                                 (first separator)
                                                 separator))
        csv* (drop ignore-lines csv)
        tx-data (map (fn [line]
                       (let [pd (case decimal-separator
                                  :dot #(Double/parseDouble %)
                                  :comma bu/parse-german-double)

                             f (ctf/formatter date-format)
                             element-order* (remove nil? element-order)
                             m (->> line
                                    (interleave element-order* ,,,)
                                    (partition 2 ,,,)
                                    (map #(into [] %) ,,,)
                                    (into {} ,,,))
                             date (or (some->> (:date m) (ctf/parse f ,,,) ctcoe/to-date)
                                      (let [{:keys [day month year]} m]
                                        (when (and day month year)
                                          (ctcoe/to-date (str year "-" month "-" day)))))
                             precip (some-> (:precip m) pd)
                             globrad (some-> (:globrad m) pd)
                             tavg (some-> (:tavg m) pd)
                             evap (or (some-> (:evap m) pd)
                                      (when (and tavg globrad)
                                        (bu/round (algo/potential-evaporation-turc-wendling globrad tavg) :digits 1)))]
                         (when (and date (or precip evap globrad tavg))
                           [:weather-station/add-data {:weather-station/id weather-station-id
                                                       :db-part (db/part user-id)
                                                       :weather-station/data (->> {:weather-data/date date
                                                                                   :weather-data/precipitation precip
                                                                                   :weather-data/evaporation evap
                                                                                   :weather-data/global-radiation globrad
                                                                                   :weather-data/average-temperature tavg}
                                                                                  (remove (comp nil? second),,,)
                                                                                  (into {},,,))}])))
                     csv*)
        tx-data* (remove nil? tx-data)]
    (try
      #_(println "tx-data*: ")
      #_(pp/pprint tx-data*)
      @(d/transact db-connection tx-data*)
      (catch Exception e
        (log/info "Couldn't import new weather-data entries! tx-data:\n" tx-data*)
        (throw e)))))


(comment "transact data to a defined db  "

  (transact-zalf-data (db/connection))

  (muencheberg-csv-data->transaction-weather-data (climate-data-muencheberg-1993-1998))

  )
