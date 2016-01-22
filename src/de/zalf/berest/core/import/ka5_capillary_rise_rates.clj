(ns de.zalf.berest.core.import.ka5-capillary-rise-rates
  (:require [datomic.api :as d]
            [de.zalf.berest.core.datomic :as db]
            [instaparse.core :as insta]
            [clojure.java.io :as cjio]
            [clojure.pprint :as pp]
            [clojure.string :as cs]
            [clojure.tools.logging :as log]
            [clojure-csv.core :as csv]))

(defn- read-and-parse-as-csv
  [file]
  (-> file
      cjio/resource
      slurp
      (csv/parse-csv :delimiter \,)))


(defn create-transaction-data
  [csv-data]
  (let [data (reduce (fn [m [_ soil-type distance-s rate-s]]
                       (let [distance (Integer/parseInt distance-s)
                             rate (* 1000 (Double/parseDouble rate-s))]
                         (assoc-in m [soil-type distance] rate)))
                     {} csv-data)
        tdata (map (fn [[soil-type dist-to-rates]]
                     {:db/id (db/new-entity-id :system)
                      :soil.type.ka5/name soil-type
                      :soil.type.ka5/capillary-rise-rates
                      (map (fn [[distance rate]]
                             {:soil.type.ka5/distance-to-groundwater-table distance
                              :soil.type.ka5/capillary-rise-rate rate})
                           dist-to-rates)})
                   data)]
    tdata))

(defn import-capillary-rise-rates-into-datomic
  [db-connection]
  (let [csv-data (read-and-parse-as-csv "private/soils/KA5a-capillary-rise-rates.csv")
        transaction-data (create-transaction-data (rest csv-data))]
    #_transaction-data
    (try
      @(d/transact db-connection transaction-data)
      (catch Exception e
        (log/info "Couldn't transact capillary rise data to datomic! data: [\n" transaction-data "\n]")
        (throw e)))))



(comment

  (import-capillary-rise-rates-into-datomic (db/connection))

  )











