(ns de.zalf.berest.core.data
  (:import [java.util Calendar])
  (:require [de.zalf.berest.core.datomic :as db]
            [clj-time.core :as ctc]
            [clojure.set :as set]
            [de.zalf.berest.core.helper :as bh :refer [rcomp]]
            [datomic.api :as d]
            [clojure.tools.logging :as log]
            [clojure.pprint :as pp]
            [clojure.string :as cs]))

(defn entity-map->map [val]
  (if (associative? val)
    (into {}
          (for [[k v] val]
            (cond
              (map? v) [k (entity-map->map v)]
              (set? v) [k (into #{} (map entity-map->map v))]
              (seq? v) [k (map entity-map->map v)]
              :else [k v])))
    val))

(defn deep-entity->map
  [db entity & {cut-references-at-map :stop-at
                incl-db-ids? :include-db-ids?}]
  (let [keep (fn [key?s]
               (if (sequential? key?s)
                 #(select-keys % key?s)
                 #(key?s %)))
        copy (fn [value]
               (if (= datomic.query.EntityMap (type value))
                 (deep-entity->map db (d/entity db (:db/id value))
                                   :stop-at cut-references-at-map
                                   :include-db-ids? incl-db-ids?)
                 value))

        m (into {} (for [[key value?s] entity]
                     [key (if-let [keep-key?s (get cut-references-at-map key)]
                            (if (set? value?s)
                              (into #{} (map (keep keep-key?s) value?s))
                              ((keep keep-key?s) value?s))
                            (if (set? value?s)
                              (into #{} (map copy value?s))
                              (copy value?s)))]))]
    (if-let [db-id (and incl-db-ids? (:db/id entity))]
      (assoc m :db/id db-id)
      m)))


(defn db->all-users
  [db & [parent-url]]
  (->> (d/q '[:find ?user-e
              :in $
              :where
              [?user-e :user/id]]
            db)
       (map #(->> % first (d/entity db ,,,)) ,,,)
       (map #(deep-entity->map db %
                               :stop-at {:user/farms [:farm/id :farm/name]
                                         :user/weather-stations [:weather-station/name
                                                                 :weather-station/id]}
                               :include-db-ids? true)
         ,,,)
       (map #(assoc % :user/password nil) ,,,)
       #_(map #(select-keys % [:db/id :user/id :user/full-name :user/farms]) ,,,)
       (map #(if parent-url
              (assoc % :url (str parent-url (:user/id %) "/"))
              %)
         ,,,)))

(defn db->a-users-weather-stations
  [db user-id & [parent-url]]
  (->> (d/q '[:find ?ws-e #_?year
              :in $ ?user-id
              :where
              [?user-e :user/id ?user-id]
              [?user-e :user/weather-stations ?ws-e]]
            db user-id)
       (map (rcomp first (partial d/entity db)) ,,,)
       (map (fn [e]
              (let [m (select-keys e [:db/id
                                      :weather-station/id
                                      :weather-station/name
                                      :weather-station/local-user-station?
                                      :weather-station/geo-coord])
                    years->elem-count (reduce (fn [m {date :weather-data/date}]
                                                (let [c (doto (Calendar/getInstance) (.setTime date))
                                                      year (.get c Calendar/YEAR)]
                                                  (assoc m year (inc (get m year 0)))))
                                              {} (:weather-station/data e))]
                (assoc m :available-years (into #{} (keys years->elem-count))
                         :years->elem-count years->elem-count))))
       (map #(if parent-url
              (assoc % :url (str parent-url (:weather-station/id %) "/"))
              %)
         ,,,)))

(defn db->all-weather-stations
  [db & [parent-url]]
  (->> (db/query-entities db :weather-station/id)
       (map #(select-keys % [#_:db/id :weather-station/id :weather-station/name :weather-station/local-user-station?]) ,,,)
       (map #(if parent-url
              (assoc % :url (str parent-url (:weather-station/id %) "/"))
              %)
         ,,,)))

(defn db->a-users-farms
  [db user-id & [parent-url]]
  (->> (d/q '[:find ?farm-e
              :in $ ?user-id
              :where
              [?user-e :user/id ?user-id]
              [?user-e :user/farms ?farm-e]]
            db user-id)
       (map #(->> % first (d/entity db ,,,)) ,,,)
       (map #(deep-entity->map db %
                               :stop-at {:farm/weather-station [:weather-station/name
                                                                :weather-station/id]
                                         :farm/authorative-weather-station [:weather-station/name
                                                                            :weather-station/id]
                                         :farm/plots :plot/id}
                               :include-db-ids? true)
         ,,,)

       (map #(if parent-url
              (assoc % :url (str parent-url (:farm/id %) "/"))
              %)
         ,,,)))

(defn db->a-farms-plots
  [db farm-id & [parent-url]]
  (->> (d/q '[:find ?plot-e
              :in $ ?farm-id
              :where
              [?farm-e :farm/id ?farm-id]
              [?farm-e :farm/plots ?plot-e]]
            db farm-id)
       (map #(->> % first (d/entity db ,,,)) ,,,)
       (map #(deep-entity->map db %
                               :stop-at {:soil.stt/substrate-groups :soil.substrate/key
                                         :crop.instance/template :crop/id}
                               :include-db-ids? true)
         ,,,)
       (map #(if parent-url
              (assoc % :url (str parent-url (:plot/id %) "/"))
              %)
         ,,,)))

(defn db->plot
  "returns pure plot-entity as Datomic EntityMap"
  [db plot-id]
  (->> (d/q '[:find ?plot-e
              :in $ ?plot-id
              :where
              [?plot-e :plot/id ?plot-id]]
            db plot-id)
       ffirst
       (d/entity db ,,,)))


(defn crop-entity->crop
  "transform the given datomic crop-entity to a crop map"
  [crop-entity & {:keys [into-sorted-map?] :or {into-sorted-map? true}}]
  (reduce (fn [m [relation key value]]
            (assoc m relation (db/kv-entities->sorted-map (relation m) key value
                                                          :into-sorted-map? into-sorted-map?)))
          (into {} crop-entity)
          [[:crop/dc-to-rel-dc-days :kv/dc :kv/rel-dc-day]
           [:crop/dc-to-developmental-state-names :kv/dc :kv/name]
           [:crop/rel-dc-day-to-cover-degrees :kv/rel-dc-day :kv/cover-degree]
           [:crop/rel-dc-day-to-extraction-depths :kv/rel-dc-day :kv/extraction-depth]
           [:crop/rel-dc-day-to-transpiration-factors :kv/rel-dc-day :kv/transpiration-factor]
           [:crop/rel-dc-day-to-quotient-aet-pets :kv/rel-dc-day :kv/quotient-aet-pet]]))

(defn db->crop-by-id
  "read a crop from the database by the given number and optional cultivation type and usage"
  [db id]
  (let [crop-e-id (ffirst (d/q '[:find ?crop-e-id
                                 :in $ ?id
                                 :where
                                 [?crop-e-id :crop/id ?id]]
                               db id))]
    (->> crop-e-id
         (db/get-entity db ,,,)
         crop-entity->crop)))

(defn db->crop-by-name
  "read a crop from the database by the given number and optional cultivation type and usage"
  [db number & {:keys [cultivation-type usage] :or {cultivation-type 0 usage 0}}]
  (let [crop-e-id (ffirst (d/q '[:find ?crop-e-id
                              :in $ ?no ?ct ?u
                              :where
                              [?crop-e-id :crop/number ?no]
                              [?crop-e-id :crop/cultivation-type ?ct]
                              [?crop-e-id :crop/usage ?u]]
                            db number cultivation-type usage))]
    (->> crop-e-id
         (db/get-entity db ,,,)
         crop-entity->crop)))

(defn db->full-selected-crops
  [db crop-ids]
  (->> (d/q '[:find ?crop-e-id
              :in $ [?crop-id ...]
              :where
              [?crop-e-id :crop/id ?crop-id]]
            db crop-ids)
       (map (rcomp first (partial d/entity db)) ,,,)
       (map #(hash-map :processed (crop-entity->crop % :into-sorted-map? false)
                       :raw %
                       :crop-type (if (nil? (:user/_crops %))
                                    :system
                                    :user))
            ,,,)))

(defn db->min-all-crops
  [db user-id & [parent-url]]
  (let [system-crops (->> (d/q '[:find ?crop-e
                                   :in $
                                   :where
                                   [?crop-e :crop/id]]
                                 db)
                         (map (rcomp first (partial d/entity db)) ,,,)
                         (filter (bh/rcomp :user/_crops nil?) ,,,)
                         (map #(select-keys % [:db/id :crop/id :crop/name :crop/symbol
                                               :crop/description
                                               :crop/number :crop/cultivation-type :crop/usage
                                               :crop/avg-additional-yield-per-mm-irrigation-water]) ,,,)
                         (map #(assoc % :crop-type :system) ,,,))
        ;_ (println "system-crops:")
        ;_ (pp/pprint system-crops)
        user-crops (->> (d/q '[:find ?crop-e
                               :in $ ?user-id
                               :where
                               [?user-e :user/id ?user-id]
                               [?user-e :user/crops ?crop-e]]
                               db user-id)
                          (map (rcomp first (partial d/entity db)) ,,,)
                          (map #(select-keys % [:db/id :crop/id :crop/name :crop/symbol
                                                :crop/description
                                                :crop/number :crop/cultivation-type :crop/usage
                                                :crop/avg-additional-yield-per-mm-irrigation-water]) ,,,)
                          (map #(assoc % :crop-type :user) ,,,))
        ;_ (println "user-crops:")
        ;_ (pp/pprint user-crops)
        ]
    (->> (concat system-crops user-crops)
         #_(map #(select-keys % [:crop-type
                               :crop/id :crop/name :crop/symbol
                               :crop/number :crop/cultivation-type :crop/usage
                               :crop/avg-additional-yield-per-mm-irrigation-water]) ,,,)
         (map #(if parent-url
                (assoc % :url (str parent-url (:crop/id %) "/"))
                %)
              ,,,)))

  #_(->> (d/q '[:find ?crop-e
              :in $
              :where
              [?crop-e :crop/id]]
            db)
       (map (rcomp first (partial d/entity db)) ,,,)
       (map #(select-keys % [:crop/id :crop/name :crop/symbol
                             :crop/number :crop/cultivation-type :crop/usage
                             :crop/avg-additional-yield-per-mm-irrigation-water]) ,,,)
       (map #(if parent-url
              (assoc % :url (str parent-url (:crop/id %) "/"))
              %)
         ,,,)))

(defn db->all-slopes
  [db & [parent-url]]
  (->> (d/q '[:find ?slope-e
              :in $
              :where
              [?slope-e :slope/key]]
            db)
       (map (rcomp first (partial d/entity db)) ,,,)
       (map #(if parent-url
              (assoc % :url (str parent-url (:slope/key %) "/"))
              %)
         ,,,)))

(defn db->all-substrate-groups
  [db & [parent-url]]
  (->> (d/q '[:find ?sg-e
              :in $
              :where
              [?sg-e :soil.substrate/key]]
            db)
       (map (rcomp first (partial d/entity db)) ,,,)
       (map #(if parent-url
              (assoc % :url (str parent-url (:soil.substrate/key %) "/"))
              %)
         ,,,)))

(defn db->all-ka5-soil-types
  [db & [parent-url]]
  (->> (d/q '[:find ?ka5-e
              :in $
              :where
              [?ka5-e :soil.type.ka5/name]]
            db)
       (map (rcomp first (partial d/entity db)) ,,,)
       (map #(if parent-url
              (assoc % :url (str parent-url (:soil.type.ka5/name %) "/"))
              %)
         ,,,)))

(defn db->all-stts
  [db & [parent-url]]
  (->> (d/q '[:find ?stt-e
              :in $
              :where
              [?stt-e :soil.stt/key]]
            db)
       (map (rcomp first (partial d/entity db)) ,,,)
       (map #(deep-entity->map db % :stop-at {:soil.stt/substrate-groups :soil.substrate/key}) ,,,)
       (map #(if parent-url
              (assoc % :url (str parent-url (:soil.stt/key %) "/"))
              %)
         ,,,)))

(defn db->all-crop->dcs
  [db]
  (->> (d/q '[:find ?crop-id ?dc ?dc-name
              :in $
              :where
              [?crop-e :crop/id ?crop-id]
              [?crop-e :crop/dc-to-developmental-state-names ?e]
              [?e :kv/dc ?dc]
              [?e :kv/name ?dc-name]]
            db)
       (reduce (fn [m [crop-id dc dc-name]]
                 (assoc-in m [crop-id dc] dc-name))
               {} ,,,)))

(defn db->donations
  [db plot-id plot-annual-id]
  (->> (d/q '[:find ?donation-e
              :in $ ?plot-id ?year
              :where
              [?plot-e :plot/id ?plot-id]
              [?plot-e :plot/annuals ?pa-e]
              [?pa-e :plot.annual/id ?plot-annual-id]
              [?pa-e :plot.annual/donations ?donation-e]]
            db plot-id plot-annual-id)
       (map (rcomp first (partial d/entity db) d/touch) ,,,)))


(defn create-new-farm
  [db-connection user-id temp-farm-name]
  (let [tx-data {:db/id (db/new-entity-id user-id)
                 :farm/id (d/squuid)
                 :farm/name temp-farm-name
                 :farm/addresses {:address/street ""}
                 :user/_farms [:user/id user-id]}]
    (try
      @(d/transact db-connection [tx-data])
      (catch Exception e
        (log/info "Couldn't create a new farm with temporary name: " temp-farm-name " tx-data: [\n" tx-data "\n]")
        (throw e)))))

(defn create-new-plot
  [db-connection user-id farm-id]
  (let [tx-data {:db/id (db/new-entity-id user-id)
                 :plot/id (d/squuid)
                 :plot/crop-area 1.0
                 :plot/irrigation-area 1.0
                 :plot/stt [:soil.stt/symbol "D1 a"]
                 :plot/slope [:slope/key 1]
                 :plot/fc-pwp-unit :soil-moisture.unit/volP
                 :farm/_plots [:farm/id farm-id]}]
    (try
      @(d/transact db-connection [tx-data])
      (catch Exception e
        (log/info "Couldn't create a new plot! tx-data: " tx-data)
        (throw e)))))

(defn create-new-plot-annual
  [db-connection user-id plot-id new-year copy-data? copy-annual-id]
  (let [db (d/db db-connection)

        copy-of-annual-data (when copy-data?
                              (some->> (d/q '[:find ?pa-e
                                              :in $ ?plot-id ?copy-annual-id
                                              :where
                                              [?p-e :plot/id ?plot-id]
                                              [?p-e :plot/annuals ?pa-e]
                                              [?pa-e :plot.annual/id ?copy-annual-id]]
                                            db plot-id copy-annual-id)
                                       ffirst
                                       (d/entity db ,,,)
                                       (#(deep-entity->map db % :stop-at {:crop.instance/template [:crop/id]}) ,,,)))

        copy-of-annual-data* (some-> copy-of-annual-data
                                     (assoc :plot.annual/crop-instances
                                            (map #(assoc % :crop.instance/template
                                                           [:crop/id (get-in % [:crop.instance/template :crop/id])]

                                                           :crop.instance/name
                                                           (cs/replace-first (:crop.instance/name %)
                                                                             (str "(" (:plot.annual/year copy-of-annual-data) ")")
                                                                             (str "(" new-year ")")))
                                                 (:plot.annual/crop-instances copy-of-annual-data)))
                                     (dissoc :plot.annual/donations
                                             :plot.annual/id
                                             #_:plot.annual/dc-assertions
                                             #_:plot.annual/abs-day-of-initial-soil-moisture-measurement
                                             #_:plot.annual/initial-soil-moistures
                                             :plot.annual/soil-moistures))

        ;_ (println "copy-of-annual-data*:")
        ;_ (pp/pprint copy-of-annual-data*)

        tx-data [(merge copy-of-annual-data*
                        {:db/id (db/new-entity-id user-id)
                         :plot.annual/id (d/squuid)
                         :plot.annual/year new-year
                         :plot.annual/description (if copy-data?
                                                    (str "(Kopie von " (:plot.annual/year copy-of-annual-data) ")")
                                                    (str "(Neues Jahr " new-year ")"))
                         :plot/_annuals [:plot/id plot-id]}
                        (when-not copy-data?
                          {:plot.annual/technology {:donation/min 1.0
                                                    :donation/max 30.0
                                                    :donation/opt 20.0
                                                    :donation/step-size 5.0
                                                    :technology/cycle-days 1
                                                    :technology/outlet-height 200
                                                    :technology/type :technology.type/sprinkler
                                                    :technology/sprinkle-loss-factor 0.2}
                           :plot.annual/initial-sm-unit :soil-moisture.unit/pFK}))]]
    (try
      #_(println "tx-data:")
      #_(pp/pprint tx-data)
      @(d/transact db-connection tx-data)
      (catch Exception e
        (log/info "Couldn't create a new annual plot! tx-data: " (pr-str tx-data))
        (throw e)))))

(defn create-new-farm-address
  [db-connection user-id farm-id]
  (let [tx-data {:db/id (db/new-entity-id user-id)
                 :address/street ""
                 :farm/_addresses [:farm/id farm-id]}]
    (try
      @(d/transact db-connection [tx-data])
      (catch Exception e
        (log/info "Couldn't create a new address for user-id: " user-id " and farm-id: " farm-id " tx-data: [\n" tx-data "\n]")
        (throw e)))))

(defn create-new-farm-contact
  [db-connection user-id farm-id]
  (let [tx-data {:db/id (db/new-entity-id user-id)
                 :person/first-name ""
                 :farm/_contacts [:farm/id farm-id]}]
    (try
      @(d/transact db-connection [tx-data])
      (catch Exception e
        (log/info "Couldn't create a new contact for user-id: " user-id " and farm-id: " farm-id " tx-data: [\n" tx-data "\n]")
        (throw e)))))

(defn create-new-soil-data-layer
  [db-connection user-id id-attr id depth type value]
  (let [[inv-entity-attr entity-attr] (case type
                                        :ism [:plot.annual/_initial-soil-moistures :soil/soil-moisture]
                                        :sm [:plot.annual.soil-moisture/_values :soil/soil-moisture]
                                        :fc [:plot/_field-capacities :soil/field-capacity]
                                        :pwp [:plot/_permanent-wilting-points :soil/permanent-wilting-point]
                                        :ka5 [:plot/_ka5-soil-types :soil/ka5-soil-type])
        tx-data [{:db/id (db/new-entity-id user-id)
                  :soil/upper-boundary-depth depth
                  entity-attr value
                  inv-entity-attr (case id-attr
                                    :db/id id
                                    [id-attr id])}]
        ]
    (try
      @(d/transact db-connection tx-data)
      (catch Exception e
        (log/info "Couldn't create a new soil data layer entry! tx-data:\n" tx-data)
        (throw e)))))

(defn create-new-donation
  [db-connection user-id annual-plot-entity-id abs-start-day abs-end-day amount]
  (let [tx-data [{:db/id (db/new-entity-id user-id)
                  :donation/abs-start-day abs-start-day
                  :donation/abs-end-day abs-end-day
                  :donation/amount amount
                  :plot.annual/_donations annual-plot-entity-id}]]
    (try
      @(d/transact db-connection tx-data)
      (catch Exception e
        (log/info "Couldn't create a new donation entry! tx-data:\n" tx-data)
        (throw e)))))

(defn create-new-soil-moisture
  [db-connection user-id annual-plot-db-id]
  (let [tx-data [{:db/id (db/new-entity-id user-id)
                  :plot.annual.soil-moisture/abs-day-of-measurement 1
                  :plot.annual.soil-moisture/unit :soil-moisture.unit/pFK
                  :plot.annual/_soil-moistures annual-plot-db-id}]]
    (try
      @(d/transact db-connection tx-data)
      (catch Exception e
        (log/info "Couldn't create a new soil-moisture entity! tx-data:\n" tx-data)
        (throw e)))))

(defn create-new-crop-instance
  [db-connection user-id annual-plot-db-id crop-template-id]
  (let [db (d/db db-connection)

        crop-name (ffirst (d/q '[:find ?name
                                 :in $ ?crop-id
                                 :where
                                 [?e :crop/id ?crop-id]
                                 [?e :crop/name ?name]]
                               db crop-template-id))

        year (or (:plot.annual/year (d/entity db annual-plot-db-id))
                 "im Jahr")

        tx-data [{:db/id (db/new-entity-id user-id)
                  :crop.instance/name (str crop-name " (" year ")")
                  :crop.instance/template [:crop/id crop-template-id]
                  :plot.annual/_crop-instances annual-plot-db-id}]]
    (try
      @(d/transact db-connection tx-data)
      (catch Exception e
        (log/info "Couldn't create a new crop-instance entry! tx-data:\n" tx-data)
        (throw e)))))

(defn create-new-dc-assertion
  [db-connection user-id crop-instance-entity-id abs-dc-day dc #_at-abs-day]
  (let [tx-data [{:db/id (db/new-entity-id user-id)
                  :assertion/abs-assert-dc-day abs-dc-day
                  :assertion/assert-dc dc
                  ;:assertion/at-abs-day at-abs-day
                  :crop.instance/_dc-assertions crop-instance-entity-id}]]
    (try
      @(d/transact db-connection tx-data)
      (catch Exception e
        (log/info "Couldn't create a new dc assertion entry! tx-data:\n" tx-data)
        (throw e)))))

(defn create-new-local-user-weather-station
  [db-connection user-id temp-weather-station-name & {:keys [local?] :or {local? true}}]
  (let [tx-data {:db/id (db/new-entity-id (if local? user-id "climate"))
                 :weather-station/id (str (d/squuid))
                 :weather-station/name temp-weather-station-name
                 :weather-station/local-user-station? local?
                 :user/_weather-stations [:user/id user-id]}]
    (try
      @(d/transact db-connection [tx-data])
      (catch Exception e
        (log/info "Couldn't create a new weather-station with temporary name: " temp-weather-station-name " tx-data: [\n" tx-data "\n]")
        (throw e)))))

(defn create-new-weather-data
  [db-connection user-id id-attr id date tavg globrad evap precip prog-date]
  (let [m {:weather-data/average-temperature (some-> tavg double)
           :weather-data/global-radiation (some-> globrad double)
           :weather-data/evaporation (some-> evap double)
           :weather-data/precipitation (some-> precip double)
           :weather-data/prognosis-date prog-date}
        rev-entity-attr (case id-attr
                          :plot/id :plot/_weather-data
                          :farm/id :farm/_weather-data
                          :weather-station/id :weather-station/_data)
        tx-data [(merge {:db/id (db/new-entity-id user-id)
                         :weather-data/date date
                         rev-entity-attr [id-attr id]}
                        (into {} (filter (comp not nil? second) m)))]]
    (try
      @(d/transact db-connection tx-data)
      (catch Exception e
        (log/info "Couldn't create a new weather-data entry! tx-data:\n" tx-data)
        (throw e)))))

(defn create-new-com-con
  [db-connection user-id contact-entity-id com-con-id com-con-desc com-con-type]
  (let [tx-data [(merge {:db/id (db/new-entity-id user-id)
                         :com-con/id com-con-id
                         :com-con/type com-con-type
                         :person/_com-connections contact-entity-id}
                        (when com-con-desc
                          {:com-con/description com-con-desc}))]]
    (try
      @(d/transact db-connection tx-data)
      (catch Exception e
        (log/info "Couldn't create a new communication connection! tx-data:\n" tx-data)
        (throw e)))))

(defn create-new-crop
  [db-connection user-id temp-name]
  (let [tx-data [{:db/id (db/new-entity-id user-id)
                  :crop/id (str (d/squuid))
                  :crop/name temp-name
                  :user/_crops [:user/id user-id]}]]
    (try
      @(d/transact db-connection tx-data)
      (catch Exception e
        (log/info "Couldn't create a new crop! tx-data:\n" tx-data)
        (throw e)))))

  (defn create-new-crop-kv-pair
  [db-connection user-id crop-id crop-attr key-attr key-value value-attr value-value]
  (let [db (d/db db-connection)
        tx-data [{:db/id (db/new-entity-id user-id)
                  (d/entid db key-attr) key-value
                  (d/entid db value-attr) value-value
                  (keyword (namespace crop-attr) (str "_" (name crop-attr))) [:crop/id crop-id]}]
        ;_ (println "tx-data: " (pr-str tx-data))
        ]
    (try
      @(d/transact db-connection tx-data)
      (catch Exception e
        (log/info "Couldn't create a new key-value pair entry for crop with id: " crop-id "! tx-data:\n" tx-data)
        (throw e)))))

(defn copy-crop
  [db-connection user-id crop-id temp-name]
  (let [db (d/db db-connection)

        crop-e (first (db/query-entities db :crop/id crop-id))

        copy (deep-entity->map db crop-e)

        tx-data [(merge copy
                        {:db/id (db/new-entity-id user-id)
                         :crop/id (str (d/squuid))
                         :crop/name temp-name
                         :user/_crops [:user/id user-id]})]]
    (try
      #_(println "tx-data: " (pr-str tx-data))
      @(d/transact db-connection tx-data)
      (catch Exception e
        (log/info "Couldn't copy crop with id: " crop-id "! tx-data:\n" tx-data)
        (throw e)))))

(defn set-substrate-group-fcs-and-pwps
  [db db-connection user-id plot-id substrate-group-key]
  (let [old-fc-es (d/q '[:find ?fc-e
                         :in $ ?plot-id
                         :where
                         [?p-e :plot/id ?plot-id]
                         [?p-e :plot/field-capacities ?fc-e]]
                       db plot-id)
        old-pwp-es (d/q '[:find ?pwp-e
                          :in $ ?plot-id
                          :where
                          [?p-e :plot/id ?plot-id]
                          [?p-e :plot/permanent-wilting-points ?pwp-e]]
                        db plot-id)
        fcs (d/q '[:find ?fc-depth ?fc
                     :in $ ?sg-key
                     :where
                     [?sg-e :soil.substrate/key ?sg-key]
                     [?sg-e :soil.substrate/field-capacities ?fc-e]
                     [?fc-e :soil/upper-boundary-depth ?fc-depth]
                     [?fc-e :soil/field-capacity ?fc]]
                   db substrate-group-key)
          pwps (d/q '[:find ?pwp-depth ?pwp
                      :in $ ?sg-key
                      :where
                      [?sg-e :soil.substrate/key ?sg-key]
                      [?sg-e :soil.substrate/permanent-wilting-points ?pwp-e]
                      [?pwp-e :soil/upper-boundary-depth ?pwp-depth]
                      [?pwp-e :soil/permanent-wilting-point ?pwp]]
                    db substrate-group-key)

          tx-data (concat
                    [[:db/add [:plot/id plot-id] :plot/fc-pwp-unit :soil-moisture.unit/volP]]
                    (for [[e] (concat old-fc-es old-pwp-es)]
                      [:db.fn/retractEntity e])
                    (for [[depth fc] fcs]
                      {:db/id (db/new-entity-id user-id)
                       :soil/upper-boundary-depth depth
                       :soil/field-capacity fc
                       :plot/_field-capacities [:plot/id plot-id]})
                    (for [[depth pwp] pwps]
                      {:db/id (db/new-entity-id user-id)
                       :soil/upper-boundary-depth depth
                       :soil/permanent-wilting-point pwp
                       :plot/_permanent-wilting-points [:plot/id plot-id]}))]
    #_tx-data
    (try
      @(d/transact db-connection tx-data)
      (catch Exception e
        (log/info "Couldn't set/copy substrate group's fcs and pwps to plot!  tx-data:\n" tx-data)
        (throw e)))))




#_(
  (:volP :pFK :pNFK) [(partial expand-layers (max-soil-depth))
                      (soil-moisture-unit {:pFK #(pFK->mm7x %1 %3)
                                           :pNFK pNFK->mm7x
                                           :volP #(-> %3
                                                      volp->mm7dm
                                                      mm7dm->mm7cm)})])

#_(defn update-fc-pwp-unit
  [db db-connection user-id plot-id new-fc-pwp-unit]
  (let [old-fc-pwp-unit (first (d/q '[:find ?unit
                                      :in $ ?plot-id
                                      :where
                                      [?p-e :plot/id ?plot-id]
                                      [?p-e :plot/fc-pwp-unit ?unit]]
                                    db plot-id))
        old-fc-es (d/q '[:find ?fc-e ?fc
                         :in $ ?plot-id
                         :where
                         [?p-e :plot/id ?plot-id]
                         [?p-e :plot/field-capacities ?fc-e]
                         [?fc-e :soil/field-capacity ?fc]]
                       db plot-id)
        old-pwp-es (d/q '[:find ?pwp-e ?pwp
                          :in $ ?plot-id
                          :where
                          [?p-e :plot/id ?plot-id]
                          [?p-e :plot/permanent-wilting-points ?pwp-e]
                          [?pwp-e :soil/permanent-wilting-point ?pwp]]
                        db plot-id)

        tx-data [{}]

        tx-data (concat
                  (for [[e] (concat old-fc-es old-pwp-es)]
                    [:db.fn/retractEntity e])
                  (for [[depth fc] fcs]
                    {:db/id (db/new-entity-id user-id)
                     :soil/upper-boundary-depth depth
                     :soil/field-capacity fc
                     :plot/_field-capacities [:plot/id plot-id]})
                  (for [[depth pwp] pwps]
                    {:db/id (db/new-entity-id user-id)
                     :soil/upper-boundary-depth depth
                     :soil/permanent-wilting-point pwp
                     :plot/_permanent-wilting-points [:plot/id plot-id]}))]
    #_tx-data
    (try
      @(d/transact db-connection tx-data)
      (catch Exception e
        (log/info "Couldn't set/copy substrate group's fcs and pwps to plot!  tx-data:\n" tx-data)
        (throw e)))))