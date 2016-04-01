(ns de.zalf.berest.core.import.dwd-data
  (:require [clojure.java.io :as cjio]
            [clojure.string :as str]
            [clj-time.core :as ctc]
            [clj-time.format :as ctf]
            [clj-time.coerce :as ctcoe]
            [clj-time.periodic :as ctp]
            [de.zalf.berest.core.datomic :as db]
            [de.zalf.berest.core.util :as bu]
            [datomic.api :as d]
            [miner.ftp :as ftp]
            [clojure.tools.logging :as log]
            [clojure.pprint :as pp]
            [clojurewerkz.quartzite.scheduler :as qs]
            [clojurewerkz.quartzite.jobs :as qj]
            [clojurewerkz.quartzite.triggers :as qt]
            #_[clojurewerkz.quartzite.date-time :as qdt]
            [clojurewerkz.quartzite.schedule.simple :as qss]
            [clojurewerkz.quartzite.schedule.cron :as qsc]
            [clojurewerkz.quartzite.schedule.daily-interval :as qsdi]))

(defn parse-prognosis-data
  "parse a DWD prognosis data file and return datomic transaction data"
  [data]
  (let [data* (str/split-lines data)
        data** (->> data* (drop 7 ,,,) (take 9))
        stations (-> data** first (str/split ,,, #"\s+"))
        prognosis-date (atom nil)]
    (for [line (drop 3 data**)
          :let [line* (str/split line #"\s+")
                date (first line*)
                date* (->> date (ctf/parse (ctf/formatter "ddMMyyyy") ,,,) ctcoe/to-date)
                _ (when-not @prognosis-date
                    (reset! prognosis-date date*))]
          [station [rr-s vp-t gs tm]] (map vector
                                           (rest stations)
                                           (partition 4 (rest line*)))]
      {:weather-station/id (str "dwd_" station)
       :weather-station/data {:weather-data/prognosis-date @prognosis-date
                              :weather-data/date date*
                              :weather-data/precipitation (bu/parse-german-double rr-s)
                              :weather-data/evaporation (bu/parse-german-double vp-t)
                              :weather-data/average-temperature (bu/parse-german-double tm)
                              :weather-data/global-radiation (bu/parse-german-double gs)}})))

(comment "instarepl debugging code"

  (def pdata
    #_(slurp "private-resources/private/climate/FY60DWLA-20140203_0915.txt"))
  (def pdata* (parse-prognosis-data pdata))
  (pp/pprint pdata*)

  )

(defn parse-measured-data
  "parse ad DWD measured data file and return ready datomic transaction data"
  [data]
  (let [data* (str/split-lines data)
        data** (->> data* (drop 6 ,,,) (take 3))
        stations (-> data** first (str/split ,,, #"\s+"))]
    (for [line (drop 2 data**)
          :let [line* (str/split line #"\s+")
                date (first line*)
                date* (->> date (ctf/parse (ctf/formatter "dd.MM.yyyy") ,,,) ctcoe/to-date)]
          [station [rr-s vp-t gs tm]] (map vector
                                           (rest stations)
                                           (partition 4 (rest line*)))]
      {:weather-station/id (str "dwd_" station)
       :weather-station/data {:weather-data/date date*
                              :weather-data/precipitation (bu/parse-german-double rr-s)
                              :weather-data/evaporation (bu/parse-german-double vp-t)
                              :weather-data/average-temperature (bu/parse-german-double tm)
                              :weather-data/global-radiation (bu/parse-german-double gs)}})))

(comment "instarepl debugging code"

  (def mdata
    #_(slurp "resources/private/climate/FY60DWLB-20130526_0815.txt")
    (slurp "resources/private/climate/FY60DWLB-20140203_0915.txt"))
  (def mdata* (parse-and-transform-measured-data mdata))
  (pp/pprint mdata*)
  (as-transaction-fns mdata*)

  )

(defn add-data
  "A transaction function creating data and just allowing unique data per station and day"
  [db data]
  (let [station-id (:weather-station/id data)
        db-part (:db-part data)
        {date :weather-data/date
         prognosis-date :weather-data/prognosis-date} (:weather-station/data data)
        q (datomic.api/q '[:find ?e
                           :in $ ?station-id ?date
                           :where
                           [?se :weather-station/id ?station-id]
                           [?se :weather-station/data ?e]
                           [?e :weather-data/date ?date]]
                         db station-id date)
        data-entities (->> q
                           (map first ,,,)
                           (map (partial datomic.api/entity db),,,))
        data-entities* (if prognosis-date
                         (filter #(= (:weather-data/prognosis-date %) prognosis-date) data-entities)
                         (filter (comp not :weather-data/prognosis-date) data-entities))
        data* (if (seq data-entities*)
                (assoc-in data [:weather-station/data :db/id] (-> data-entities* first :db/id))
                data)
        data** (dissoc data* :db-part)]
    ;always create a temporary db/id, will be upsert if station exists already
    [(assoc data** :db/id (datomic.api/tempid (or db-part :berest.part/climate)))]))


(comment "insert transaction function into db, without full schema reload"

  @(d/transact (db/connection)
            [(read-string "{:db/id #db/id[:berest.part/climate]
  :db/ident :weather-station/add-data
  :db/doc \"A transaction function creating data and just allowing unique data per station and day\"
  :db/fn #db/fn {:lang \"clojure\"
                 :params [db data]
                 :code \"(let [station-id (:weather-station/id data)
                 db-part (:db-part data)
       {date :weather-data/date
         prognosis-date :weather-data/prognosis-date} (:weather-station/data data)
        q (datomic.api/q '[:find ?e
                           :in $ ?station-id ?date
                           :where
                           [?se :weather-station/id ?station-id]
                           [?se :weather-station/data ?e]
                           [?e :weather-data/date ?date]]
                         db station-id date)
        data-entities (->> q
                           (map first ,,,)
                           (map (partial datomic.api/entity db),,,))
        data-entities* (if prognosis-date
                         (filter #(= (:weather-data/prognosis-date %) prognosis-date) data-entities)
                         (filter (comp not :weather-data/prognosis-date) data-entities))
        data* (if (seq data-entities*)
                (assoc-in data [:weather-station/data :db/id] (-> data-entities* first :db/id))
                data)
                data** (dissoc data* :db-part)]
    ;always create a temporary db/id, will be upsert if station exists already
    [(assoc data** :db/id (datomic.api/tempid (or db-part :berest.part/climate)))])\"}}")])

  )


(comment "instarepl test"

  (add-data (db/current-db) {:weather-station/id "dwd_10490",
                             :weather-station/data
                             {:weather-data/prognosis-date #inst "2014-08-29T00:00:00.000-00:00"
                              :weather-data/date #inst "2014-08-29T00:00:00.000-00:00"
                              :weather-data/precipitation 0.0
                              :weather-data/evaporation 3.9
                              :weather-data/average-temperature 14.5
                              :weather-data/global-radiation 2124.0}})

  (datomic.api/q '[:find ?se ?station-id
                 :in $
                 :where
                 [?se :weather-station/id ?station-id]
                 #_[?se :weather-station/data ?e]
                 #_[?e :weather-data/date ?date]]
               (db/current-db) "dwd_10162" #inst "2014-02-04T00:00:00.000-00:00")

  )

(def kind-pattern #"FY60DWL(A|B)-\d{8}_\d{4}.txt")

(def date-pattern #"FY60DWL\w-(\d{8})_(\d{4}).txt")

#_(defn make-filename [kind date & {:keys [h min] :or {h 9, min 15}}]
  (str "FY60DWL" ({:prognosis "A"
                   :measured "B"} kind)
       "-" (ctf/unparse (ctf/formatter "yyyyMMdd") date) "_" (format "%02d%02d" h min) ".txt"))

(comment "instarepl debug code"

  (make-prognosis-filename (ctc/date-time 2013 6 3))

  ;real ftp seams to be not necessary for just getting data (at least for anonymous access and co)
  (def t (ftp/with-ftp [client (System/getProperty "import.ftp.dwd.url")]
                       (ftp/client-get-stream client (make-prognosis-filename (ctc/date-time 2013 6 3)))))

  (clojure.java.io/reader t)

  )

(comment

  (def files (ftp/list-files (System/getProperty "import.ftp.dwd.url")))

  )

(defn import-dwd-data-into-datomic*
  "import the dwd data under the given url [and filename] into datomic"
  ([ftp-url filename]
   (try
     (let [ftp-url-to-filename (str ftp-url filename)
           kind-identifier (second (re-find kind-pattern filename))
           data (try
                  (slurp ftp-url-to-filename)
                  (catch Exception e
                    (log/info (str "Couldn't read file from ftp server! URL was " ftp-url-to-filename))
                    (throw e)))
           transaction-data (case kind-identifier
                              "A" (parse-prognosis-data data)
                              "B" (parse-measured-data data))

           ;_ (println "transaction-data: " (pr-str transaction-data))

           ;insert transaction data via :weather-station/add-data transaction function, to create unique data per station and day
           transaction-data->add-data (map #(vector :weather-station/add-data %) transaction-data)
           ;_ (println "transaction-data->add-data: " (pr-str transaction-data->add-data))
           ]
       (try
         @(d/transact (db/connection) transaction-data->add-data)
         (catch Exception e
           (println "Exception e: " (pr-str e))
           (println #_log/info "Couldn't write dwd data to datomic! data: [\n" (pr-str transaction-data->add-data) "\n]")
           (throw e)))
       true)
     (catch Exception _ false))))

(defn import-dwd-data-into-datomic
  "import the dwd data at the given dates into datomic"
  [& dates]
  (let [dates* (or dates [(ctc/now)])
        dates** (map #(ctf/unparse (ctf/formatter "yyyyMMdd") %) dates*)
        ;_ (println "dates**: " dates**)
        url (System/getProperty "import.ftp.dwd.url")
        all-files (ftp/list-files url)
        ;_ (println "all-files: " all-files)
        grouped-files (group-by #(second (re-find date-pattern %)) all-files)
        ;_ (println "grouped-files: " grouped-files)
        grouped-files-at-dates (select-keys grouped-files dates**)
        ;_ (println "grouped-files-at-dates: " grouped-files-at-dates)
        ]
    (doseq [[d files-at-date] grouped-files-at-dates
            file-at-date files-at-date]
      (import-dwd-data-into-datomic* (str/replace-first url "anonymous@" "") file-at-date))))

(defn bulk-import-dwd-data-into-datomic
  [from-date to-date]
  (apply import-dwd-data-into-datomic
         (take (ctc/in-days (ctc/interval from-date (ctc/plus to-date (ctc/days 1))))
               (ctp/periodic-seq from-date (ctc/days 1)))))

(comment

  (import-dwd-data-into-datomic (ctc/date-time 2014 2 3))

  (bulk-import-dwd-data-into-datomic (ctc/date-time 2014 4 19)
                                     (ctc/date-time 2014 8 20))

  )

(def scheduler (atom nil))

(qj/defjob
  ImportDWDData
  [ctx]
  (import-dwd-data-into-datomic))

(def dwd-import-job-key "jobs.import-dwd-data")
(def dwd-import-trigger-key "triggers.import-dwd-data")

(defn- schedule-dwd-import
  [hour min]
  (let [job (qj/build
              (qj/of-type ImportDWDData)
              (qj/with-identity (qj/key dwd-import-job-key)))
        trigger (qt/build
                  (qt/with-identity (qt/key dwd-import-trigger-key))
                  (qt/start-now)
                  (qt/with-schedule (qsdi/schedule
                                      (qsdi/every-day)
                                      #_(qsdi/with-interval-in-seconds 10)
                                      (qsdi/starting-daily-at (qsdi/time-of-day hour min 0))
                                      (qsdi/ending-daily-at (qsdi/time-of-day (inc hour) min 0)))))]
    (qs/schedule @scheduler job trigger)))

(defn start-import-scheduler
  []
  (reset! scheduler (qs/start (qs/initialize)))
  (println "println starting dwd-import-scheduler")
  (log/info "starting dwd-import-scheduler")
  (let [[hour min] (first (d/q '[:find ?hour ?min
                                 :in $
                                 :where
                                 [?e :settings.import.dwd/at-hour ?hour]
                                 [?e :settings.import.dwd/at-minute ?min]]
                               (db/current-db)))]
    (println "at " hour ":" min)
    (schedule-dwd-import hour min)))


(defn stop-import-scheduler
  []
  (qs/delete-trigger @scheduler (qt/key dwd-import-trigger-key))
  (qs/shutdown @scheduler)
  (println "stopping dwd-import-scheduler")
  (log/info "stopping dwd-import-scheduler"))

(defn set-import-time-settings
  ([hour min] (set-import-time-settings (db/connection) hour min))
  ([db-connection hour min]
   (let [tx-data {:db/id :settings.import.dwd/time #_(db/new-entity-id (db/system-part))
                  :settings.import.dwd/at-hour hour
                  :settings.import.dwd/at-minute min}]
     #_(println "hour: " hour " min: " min " tx-data: " tx-data)
     (try
       @(d/transact db-connection [tx-data])
       (println "Updated DWD import time to: " hour ":" min " daily.
       This will have effect upon restarting app, if rescheduling fails.")
       (log/info "Updated DWD import time to: " hour ":" min " daily.
       This will have effect upon restarting app, if rescheduling fails.")
       (qs/delete-trigger @scheduler (qt/key dwd-import-trigger-key))
       (schedule-dwd-import hour min)
       (println "Updated scheduler for new DWD import time to: " hour ":" min " daily.")
       (log/info "Updated scheduler for new DWD import time to: " hour ":" min " daily.")
       (catch Exception e
         (println "Couldn't update DWD import time! data: [\n" tx-data "\n]")
         (log/info "Couldn't update DWD import time! data: [\n" tx-data "\n]"))))))
