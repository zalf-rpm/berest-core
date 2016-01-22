(ns de.zalf.berest.core.queries
  (:require [clojure.string :as cs]
            [clojure.edn :as edn]
            [datomic.api :as d]
            [de.zalf.berest.core.datomic :as db]))




(defmulti string->value* (fn [db-type _] db-type))

(defmethod string->value* :default [_ value]
  nil)

(defmethod string->value* :db.type/double [_ value]
  (-> value edn/read-string double))

(defmethod string->value* :db.type/string [_ value]
  value)

(defmethod string->value* :db.type/long [_ value]
  (-> value edn/read-string long))

(defmethod string->value* :db.type/float [_ value]
  (-> value edn/read-string float))

(defmethod string->value* :db.type/double [_ value]
  (-> value edn/read-string double))

(defmethod string->value* :db.type/bigint [_ value]
  (-> value BigInteger.))

(defmethod string->value* :db.type/bigdec [_ value]
  (-> value BigDecimal.))

(defmethod string->value* :db.type/instant [_ value]
  (-> value edn/read-string java.util.Date))

(defmethod string->value* :db.type/boolean [_ value]
  (-> value edn/read-string))

(defn string->value [db attr str-value]
  (let [db-type (ffirst
                  (d/q '[:find ?db.type
                         :in $ ?attr
                         :where
                         [?ws-e :db/ident ?attr]
                         [?ws-e :db/valueType ?db.type-e]
                         [?db.type-e :db/ident ?db.type]]
                       db attr))]
    (string->value* db-type str-value)))

#_(string->value (db/current-db) :geo-coord/latitude "111.")


(defn get-entities [db id-attr]
  (let [result (d/q '[:find ?e
                      :in $ ?id-attr
                      :where
                      [?e ?id-attr]]
                    db id-attr)]
    (->> result
         (map first ,,,)
         (map (partial d/entity db) ,,,))))


(defn get-ui-entities
  "return ui entities as a sorted sequence according to :rest.ui/order-no"
  [db attr & [value]]
  (let [result (if value
                 (d/q '[:find ?ui-e ?d-attr-e
                        :in $ ?attr ?value
                        :where
                        [?ui-e ?attr ?value]
                        [?ui-e :rest.ui/describe-attribute ?d-attr-e]]
                      db (d/entid db attr) value)
                 (d/q '[:find ?ui-e ?d-attr-e
                        :in $ ?attr
                        :where
                        [?ui-e ?attr]
                        [?ui-e :rest.ui/describe-attribute ?d-attr-e]]
                      db (d/entid db attr)))]
    (->> result
         (map #(merge {}
                      (d/entity db (first %))
                      (d/entity db (second %)))
           ,,,)
         (sort-by :rest.ui/order-no ,,,)
         #_(map d/touch ,,,))))


