(ns de.zalf.berest.core.datomic
  (:require clojure.set
            [crypto.password.scrypt :as pwd]
            #_[buddy.sign.generic :as sign]
            [clojure.java.io :as cjio]
            [clojure.tools.logging :as log]
            [datomic.api :as d]
            #_[datomic.client.api :as d]
            [de.zalf.berest.core.util :as bu]
            [de.zalf.berest.core.helper :as bh :refer [rcomp]]))

(def ^:dynamic *db-id* "berest")

#_(def cfg {:server-type :dev-local
          :system "weberest"})
#_(def client (d/client {:server-type :dev-local
                       :system "datomic-samples"}))
#_(def client (d/client cfg))

(def partition-namespace #(str *db-id* ".part"))

(defn part
  "create a partition name using the *db-id* if arg is no namespaced keyword,
  else assume arg was already a correct partition name"
  [part-name]
  (if (and (keyword? part-name) (namespace part-name))
    part-name
    (keyword (partition-namespace) (name part-name))))

(def system-part #(part "system"))
(def climate-part #(part "climate"))

(defn datomic-connection-string* [base-uri db-id]
  (str base-uri db-id))

(def dev-local-base-uri "datomic:dev://localhost:4334/")
#_(def free-local-base-uri "datomic:free://localhost:4334/")
#_(def free-local-connection-string (partial datomic-connection-string* free-local-base-uri))

#_(def infinispan-local-datomic-base-uri "datomic:inf://localhost:11222/")
#_(def infinispan-local-connection-string (partial datomic-connection-string* infinispan-local-datomic-base-uri))

#_(def datomic-connection-string #_dynamodb-connection-string free-local-connection-string)
(def datomic-connection-string (partial datomic-connection-string* dev-local-base-uri #_(System/getProperty "berest.datomic.url")))

(defn connection [& [db-id]]
  (->> (or db-id *db-id*)
       datomic-connection-string
       #_(#(do (println "datomic-connection-string: " %) %))
       d/connect))

(defn current-db [& [db-id]]
  (some->> (or db-id *db-id*)
           connection
           d/db))

(defn current-db-test []
  (d/connect "datomic:dev://localhost:4334/berest"))

(def datomic-schema-files ["private/db/standard-partitions-schema.edn"
                           "private/db/meta-schema.edn"

                           "private/db/settings.edn"
                           "private/db/user-schema.edn"

                           "private/db/key-value-pair-schema.edn"
                           "private/db/farm-schema.edn"
                           "private/db/person-schema.edn"
                           "private/db/address-schema.edn"
                           "private/db/communication-schema.edn"
                           "private/db/geo-coord-schema.edn"
                           "private/db/crop-schema.edn"
                           "private/db/crop-instance-schema.edn"
                           "private/db/plot-schema.edn"
                           "private/db/plot-annual-schema.edn"
                           "private/db/dc-assertion-schema.edn"
                           "private/db/technology-and-donation-schema.edn"
                           "private/db/soil-schema.edn"
                           "private/db/climate-schema.edn"

                           "private/db/rest-ui-description.edn"])

(defn apply-schemas-to-db
  "returns true if successful and no errors happend during transactions"
  [db-connection & schema-files]
  (let [errors (atom false)]
    (doseq [[schema-file tx-data] (map vector
                                       schema-files
                                       (map (rcomp cjio/resource slurp read-string) schema-files))]
      (try
        @(d/transact db-connection tx-data)
        (catch Exception e
          (println "Exception trying to transact data from schema-file: " schema-file "! Exception: " e)
          (reset! errors true))))
    (not @errors)))

(defn delete-db!
  [db-id]
  (->> db-id
       datomic-connection-string
       d/delete-database))

(defn create-db
  [db-id & initial-schema-files]
  (let [uri* (datomic-connection-string db-id)
        res {:db-id db-id
             :db-uri uri*}]
    (try
      (if (d/create-database uri*)
        (try
          (assoc res :success (apply apply-schemas-to-db (d/connect uri*) initial-schema-files))
          (catch Exception e
            (log/info "Couldn't apply schemas to db at uri: " uri* ", schema files where: "
                      initial-schema-files ". Removing db.")
            (delete-db! db-id)
            (assoc res :success false
                       :error-reason :exception-applying-schemas)))
        (assoc res :success false
                   :error-reason :db-alredy-existed))
      (catch Exception e
        (log/info "Couldn't create db at uri: " uri* ". Error was: " e)
        (assoc res :success false
                   :error-reason :exception-db-creation)))))

(comment
  "instarepl debugging code"

  (apply create-db *db-id* datomic-schema-files)
  (d/create-database (datomic-connection-string *db-id*))
  (apply apply-schemas-to-db (connection *db-id*)
         (apply concat (vals datomic-schema-files)))
  (delete-db! *db-id*)


  (def ss (-> datomic-schema-files :system second))
  (def ss* ((bh/rcomp cjio/resource slurp read-string) ss))
  (d/transact (connection "system") ss*)

  (def ms (-> datomic-schema-files :berest first))
  (def ms* ((bh/rcomp cjio/resource slurp read-string) ms))
  (d/transact (connection "system") ms*)

  (def s (-> datomic-schema-files :berest second))
  (def s* ((bh/rcomp cjio/resource slurp read-string) s))
  (d/transact (connection "system") s*)

  (def rui (-> datomic-schema-files :berest (nth ,,, 2)))
  (def rui* ((bh/rcomp cjio/resource slurp read-string) rui))
  (d/transact (connection "system") rui*)

  )

(defn new-entity-ids [in-partition] (repeatedly #(d/tempid (part in-partition))))
(defn new-entity-id [in-partition] (first (new-entity-ids in-partition)))
(defn temp-entity-id [in-partition value] (d/tempid (part in-partition) value))

(defn create-entities
  ([in-partition key value kvs]
    (map (fn [id [k v]] {:db/id id
                         key k
                         value v})
         (new-entity-ids in-partition) (apply array-map kvs)))
  ([in-partition ks-to-vss]
    (map #(assoc (zipmap (keys ks-to-vss) %) :db/id (new-entity-id in-partition))
         (apply map vector (vals ks-to-vss)))))

(defn create-inline-entities
  ([key value kvs]
   (mapv (fn [[k v]] {key k, value v})
        (apply array-map kvs)))
  ([ks-to-vss]
   (mapv #(zipmap (keys ks-to-vss) %)
        (apply map vector (vals ks-to-vss)))))

(defn get-entity-ids [entities] (mapv :db/id entities))
(defn get-entity-id [entity] (:db/id entity))

(defn get-entities [db entity-ids]
  (map (partial d/entity db) entity-ids))

(defn get-entity [db entity-id]
  (first (get-entities db [entity-id])))

(defn create-map-from-entity-ids
  [db key value entity-ids]
  (->> entity-ids
    (get-entities db ,,,)
    (map (juxt key value) ,,,)
    (into (sorted-map) ,,,)))

(defn kv-entities->sorted-map
  [entities key value & {:keys [into-sorted-map?] :or {into-sorted-map? true}}]
  (->> entities
    (map (juxt key value) ,,,)
    (into (if into-sorted-map? (sorted-map) {}) ,,,)))

(defn query-entities
  ([db attr]
   (map (rcomp first (partial d/entity db))
        (d/q '[:find ?e
               :in $ ?id-attr
               :where
               [?e ?id-attr]]
             db (d/entid db attr))))
  ([db attr value]
  (map (rcomp first (partial d/entity db))
       (d/q '[:find ?e
              :in $ ?attr ?value
              :where
              [?e ?attr ?value]]
            db (d/entid db attr) value))))

(defn create-dc-assertion*
  "Create a dc assertion for given year 'in-year' to define that at abs-dc-day
  the dc-state was 'dc'. Optionally a at-abs-day can be given when the
  dc state had been told the system, else abs-dc-day will be assumed."
  [in-partition in-year abs-dc-day dc & [at-abs-day]]
  {:db/id (new-entity-id in-partition)
   ;:assertion/at-abs-day (or at-abs-day abs-dc-day)
   :assertion/assert-dc dc
   :assertion/abs-assert-dc-day abs-dc-day})

(defn create-dc-assertion
  "Create a dc assertion for given year 'in-year' to define that at '[day month]'
  the dc-state was 'dc'. Optionally a '[at-day at-month]' can be given when the
  dc state had been told the system, else '[day month]' will be assumed"
  [in-partition in-year [day month] dc & [[at-day at-month :as at]]]
  (let [abs-dc-day (bu/date-to-doy day month in-year)
        #_at-abs-day #_(if (not-any? nil? (or at [nil]))
                     (bu/date-to-doy at-day at-month in-year)
                     abs-dc-day)]
    (create-dc-assertion* in-partition in-year abs-dc-day dc #_at-abs-day)))

(defn create-dc-assertions
  "create multiple assertions at one"
  [in-partition in-year assertions]
  (map #(apply create-dc-assertion in-partition in-year %) assertions))


(defn create-irrigation-donation
	"Create datomic map for an irrigation donation given an start-abs-day
	and the irrigation-donation in [mm]"
  [in-partition in-year [start-day start-month] [end-day end-month] donation-mm]
  (let [abs-start-day (bu/date-to-doy start-day start-month in-year)
        abs-end-day (bu/date-to-doy end-day end-month in-year)]
    {:db/id            (new-entity-id in-partition)
     :donation/abs-start-day abs-start-day
     :donation/abs-end-day abs-end-day
     :donation/amount  donation-mm}))

(defn create-irrigation-donations
  "Create multiple irrigation donation datomic maps at once"
  [in-partition in-year donations]
  (map #(apply create-irrigation-donation in-partition in-year %) donations))


;;transaction functions

(comment "moved the only transaction function to dwd_data.clj as it is actually pretty domain specific")


;; user management and credential functions

(defn store-credentials
  "store given credentials into db"
  [db-connection user-id password full-name roles]
  (let [enc-pwd (pwd/encrypt password)
        kw-roles (map #(->> % name (keyword "user.role" ,,,)) roles)
        creds {:db/id (new-entity-id :system)
               :user/id user-id
               :user/password enc-pwd
               :user/full-name full-name
               :user/roles kw-roles}
        creds- (dissoc creds :db/id :user/password)]
    (try
      @(d/transact db-connection [creds])
      creds-
      (catch Exception e
        (log/info "Couldn't store credentials into datomic database! Data w/o pwd: [\n"
                  (dissoc creds :user/password) "\n]")
        nil))))

(defn update-password
  "update a users password"
  [db-connection user-id new-password]
  (let [enc-pwd (pwd/encrypt new-password)
        transaction-data [:db/add [:user/id user-id] :user/password enc-pwd]]
    (try
      @(d/transact db-connection [transaction-data])
      true
      (catch Exception e
        (log/info #_println "Couldn't update the users password! Data w/o pwd: [\n"
                  (assoc transaction-data 3 "xxx") "\n]")
        nil))))

(defn update-user-roles
  "update a users roles at once (replace old roles with new roles"
  [db-connection user-id new-roles]
  (let [old-roles (d/q '[:find ?role
                         :in $ ?user-id
                         :where
                         [?e :user/id ?user-id]
                         [?e :user/roles ?role]]
                       (d/db db-connection) user-id)

        retraction-transaction-data (for [[role-e] old-roles]
                                      [:db/retract [:user/id user-id] :user/roles role-e])

        transaction-data {:db/id [:user/id user-id]
                          :user/roles new-roles}]
    (try
      @(d/transact db-connection retraction-transaction-data)
      @(d/transact db-connection [transaction-data])
      true
      (catch Exception e
        (log/info #_println "Couldn't update the users roles! retraction-transaction-data of old roles: [\n"
                 retraction-transaction-data
                 " transaction-data for new roles: " transaction-data "\n]")
        nil))))

(defn add-user-weather-stations
  "update a users roles at once (replace old roles with new roles"
  [db-connection user-id new-weather-station-ids]
  (let [transaction-data {:db/id [:user/id user-id]
                          :user/weather-stations (for [id new-weather-station-ids]
                                                   [:weather-station/id id])}]
    (try
      @(d/transact db-connection [transaction-data])
      true
      (catch Exception e
        (println "Couldn't add new user weather-stations! transaction-data: [\n" (pr-str transaction-data) "\n]")
        nil))))

(defn remove-user-weather-stations
  "update a users roles at once (replace old roles with new roles"
  [db-connection user-id weather-station-ids]
  (let [transaction-data (for [id weather-station-ids]
                           [:db/retract [:user/id user-id] :user/weather-stations [:weather-station/id id]])]
    (try
      @(d/transact db-connection transaction-data)
      true
      (catch Exception e
        (log/info #_println "Couldn't remove user weather-stations! transaction-data: [\n" (pr-str transaction-data) "\n]")
        nil))))


(comment

  (store-credentials (connection) "michael" "..." "Michael Berg" [:admin :guest :farmer :consultant])
  (store-credentials (connection) "guest" "guest" "Guest Guest" [:guest])
  (store-credentials (connection) "zalf" "..." "Zalf Zalf" [:consultant])

  )



(defn create-partition
  "partition-name should be either a string or keyword"
  [db-connection partition-name]
  (let [tx-data {:db/id (d/tempid :db.part/db) ;#db/id[:db.part/db]
                 :db/ident (part partition-name)
                 :db.install/_partition :db.part/db}]
    (try
      @(d/transact db-connection [tx-data])
      partition-name
      (catch Exception e
        (log/info "Couldn't create a new partition: " partition-name " tx-data: [\n" tx-data "\n]")
        nil))))


(defn register-user
  "registering means, that additionally to just storing the credentials
   a new partition will be created for the user's data"
  ([user-id password full-name]
   (register-user (connection) user-id password full-name [:guest]))
  ([user-id password full-name roles]
   (register-user (connection) user-id password full-name roles))
  ([db-connection user-id password full-name roles]
   (when-let [creds (store-credentials db-connection user-id password full-name roles)]
     (if (create-partition db-connection user-id)
       creds
       (try
         @(d/transact (db-connection) [[:db.fn/retractEntity [:user/id (:user/id creds)]]])
         nil
         (catch Exception e
           (log/info "Couldn't retract newly created user entity with credentials: \n" creds
                     "\nafter failing to install partition: " user-id ".")
           nil))))))

(comment

  (register-user "michael" "..." "Michael Berg" [:admin :guest :farmer :consultant])
  (register-user "guest" "guest" "Guest Guest")
  (register-user "zalf" "..." "Zalf Zalf" [:consultant])

  )

(defn- get-user-entity
  "get the entity stored for the given user"
  [db user-id]
  (some->> (d/q '[:find ?e
                  :in $ ?user-id
                  :where
                  [?e :user/id ?user-id]]
                db user-id)
           ffirst
           (d/entity db ,,,)))

(defn credentials*
  [db user-id password]
  (if-let [user-entity (get-user-entity db user-id)]
    (when true #_(pwd/check password (:user/password user-entity))
      (select-keys user-entity [:user/id :user/roles :user/full-name]))))

(defn credentials
  ([{:keys [username password]}]
   (credentials username password))
  ([user-id password]
   (credentials* (current-db) user-id password)))


(comment
  "insta repl code"

  (credentials* (current-db "berest") "michael" "...")

  (d/q '[:find ?e
         :in $
         :where
         [?e :user/id ?user-id]]
       (current-db) "michael")

  (->> (d/q '[:find ?e
              :in $
              :where
              [?e :user/id ?user-id]]
            (current-db) "michael")
       ffirst
       (d/entity (current-db) ,,,)
       d/touch)

  (->> (d/q '[:find ?e
              :in $
              :where
              [?e :db/ident :user/id]]
            (current-db))
       ffirst
       (d/entity (current-db))
       d/touch)


  )

#_(defn create-session-token
  ([user-id]
   (create-session-token (current-db) user-id))
  ([db user-id]
   (if-let [enc-pwd (some->> (get-user-entity db user-id) :user/password)]
     (sign/sign user-id enc-pwd))))

#_(defn check-session-token
  "check if the given token is valid given the user-id and an optional timeout time
  max-age in seconds, returns (like credentials) the user-identity if valid"
  ([token]
   (check-session-token (first (str/split token #":")) token nil))
  ([user-id token]
   (check-session-token user-id token nil))
  ([user-id token max-age]
   (check-session-token (current-db) user-id token max-age))
  ([db user-id token max-age]
   (let [user-entity (get-user-entity db user-id)
         enc-pwd (:user/password user-entity)]
     (when (and enc-pwd
                (sign/unsign token enc-pwd {:max-age (or max-age nil)}))
       (select-keys user-entity [:user/id :user/roles :user/full-name])))))




