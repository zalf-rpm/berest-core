(ns de.zalf.berest.core.import.berest-90-crops
  (:require [datomic.api :as d]
            [de.zalf.berest.core.datomic :as db]
            [instaparse.core :as insta]
            [clojure.java.io :as cjio]
            [clojure.pprint :as pp]
            [clojure.string :as cs]
            [clojure.tools.logging :as log]))

(def crop-block-parser
  (insta/parser
   "
   crop-block = <empty-line*> (crop-data <empty-line*>)+ <ows-without-newline EOF>

   <crop-data> = header-line | dc-2-rel-day | dc-2-name | dc-2-coverdegree | dc-2-extraction-depth |
   dc-2-transpiration | dc-2-quotient | dc-2-effectivity | dito

   (*
   * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * -
   block-separator = <ows ('*' ows '-' ows '*'?)+ ows-without-newline newline>
   *)

   (*
   0101,7,0,WW,Winterweizen/AJ;      Aussaatjahr
   *)
   (*header-line = <ows> crop-no <','> cult-type <','> (usage <','> crop-code <','>)? crop-name <ows> <rest-of-line>*)
   header-line = <ows> crop-no <','> cult-type <','> usage <','> crop-code <','> crop-name <ows> <rest-of-line>
   crop-no = #'[0-9]+'
   cult-type = integer
   usage = integer
   crop-code = #'[^,]*'
   crop-name = #'[^;]*'

   (*
   DC =    1,  10, 21;  Code
           1,  15, 72;  Tag
   *)
   dc-2-rel-day =
   <ows 'DC' ows '=' ows> integer-values <rest-of-line>
   <ows> integer-values <rest-of-line>

   integer-values = (integer <ows> <','>? <ows>)+
   double-values = (double <ows> <','>? <ows>)+

   (*
   NameDC =   1 : Aussaat;
             10 : Aufgang;
             21 : Best.-beginn;
   *)
   dc-2-name = <ows 'NameDC' ows '=' ows> dc-2-name-pairs
   dc-2-name-pairs = dc-2-name-pair+
   <dc-2-name-pair> = <ows> integer <ows ':' ows> #'[^;]+' <rest-of-line>

   (*
   Bedeckungsgrad   =   15,   30,  115;                    Tag
                         0, 0.60, 0.80;                    Wert
   *)
   dc-2-coverdegree =
   <ows 'Bedeckungsgrad' ows '=' ows> integer-values <rest-of-line>
   <ows> double-values <rest-of-line>

   (*
   Entnahmetiefe    =   10,  90;                           Tag
                         1,   6;                           Wert
   *)
   dc-2-extraction-depth =
   <ows 'Entnahmetiefe' ows '=' ows> integer-values <rest-of-line>
   <ows> integer-values <rest-of-line>

   (*
   Transpiration    =    1;                                Tag
                         1;                                Wert
   *)
   dc-2-transpiration =
   <ows 'Transpiration' ows '=' ows> integer-values <rest-of-line>
   <ows> double-values <rest-of-line>

   (*
   Quotient(soll)   =    1;                                Tag
                         0;                                Wert
   *)
   dc-2-quotient =
   <ows 'Quotient' ows '(soll)' ows '=' ows> integer-values <rest-of-line>
   <ows> double-values <rest-of-line>

   (*
   Effektivitaet    =    1;                                Tag
                      0.17;                                Wert
   *)
   dc-2-effectivity =
   <ows 'Effektivitaet' ows '=' ows> integer-values <rest-of-line>
   <ows> double-values <rest-of-line>

   (*
   0101,7,2,dito.,;
   0101,7,3,dito.,;
   0101,7,9,dito.,;
   *)
   dito = <ows> crop-no <','> cult-type <','> usage <',' ows 'dito.' ows ',' rest-of-line>

   rest-of-line = ';' #'[^\\n\\r]*' (newline | EOF)
   empty-line = newline | ws-without-newline newline
   newline = '\\r\\n' | '\\n'
   ows-without-newline = #'[^\\S\\n\\r]*'
   ws-without-newline = #'[^\\S\\n\\r]+'
   ows = #'\\s*'
   ws = #'\\s+'
   word = #'[a-zA-Z0-9/.-]+'
   integer = #'[0-9]+'
   double = #'[0-9]+(?:\\.[0-9]*)?'
   SOF = #'\\A'
   EOF = #'\\Z'
   "))


#_(def test-text
   "
0000,0,0,BRACHE,Brache;

DC =    1;  Code
        1;  Tag

NameDC =  1: Brache;


Bedeckungsgrad =   1;          Tag
                   0;          Wert

Entnahmetiefe  =   1;          Tag
                   1;          Wert

Transpiration  =   1;          Tag
                   1;          Wert

Quotient(soll) =   1;          Tag
                   0;          Wert

Effektivitaet  =   1;          Tag
                   0;          Wert

")


(comment

  (crop-block-parser test-text)

  (def ps (insta/parses crop-block-parser test-text))
                        ;:total true

  (pp/pprint ps)
  (count ps)
  (pp/pprint (nth ps 0)))



(defn transform-crop-block [block]
  (let [trans {:double #(Double/parseDouble %)
               :integer #(Integer/parseInt %)
               :double-values vector
               :integer-values vector
               :word identity

               :crop-no identity
               :cult-type identity
               :usage identity
               :crop-code cs/trim
               :crop-name cs/trim

               :header-line (fn [crop-no cult-type & [usage-or-crop-name crop-code crop-name]]
                              [:crop {:db/id (db/new-entity-id (db/system-part))
                                      :crop/id (str crop-no "-" cult-type (when usage-or-crop-name "-") usage-or-crop-name)
                                      :crop/number (Integer/parseInt crop-no)
                                      :crop/cultivation-type cult-type
                                      :crop/usage usage-or-crop-name
                                      :crop/name crop-name
                                      :crop/symbol (or crop-code crop-name)}])

               :dc-2-rel-day (fn [dcs days]
                               [:dc-2-rel-day (db/create-entities (db/system-part)
                                                                  :kv/dc :kv/rel-dc-day
                                                                  (interleave dcs days))])

               :dc-2-coverdegree (fn [dcs cds]
                                   [:dc-2-coverdegree (db/create-entities (db/system-part)
                                                                          :kv/rel-dc-day :kv/cover-degree
                                                                          (interleave dcs cds))])

               :dc-2-name-pairs vector
               :dc-2-name (fn [pairs]
                            [:dc-2-name (db/create-entities (db/system-part)
                                                            :kv/dc :kv/name
                                                            pairs)])

               :dc-2-extraction-depth (fn [dcs cds]
                                        [:dc-2-extraction-depth (db/create-entities (db/system-part)
                                                                                    :kv/rel-dc-day :kv/extraction-depth
                                                                                    (interleave dcs (map #(* % 10) cds)))])

               :dc-2-transpiration (fn [dcs cds]
                                     [:dc-2-transpiration-factor (db/create-entities (db/system-part)
                                                                                     :kv/rel-dc-day :kv/transpiration-factor
                                                                                     (interleave dcs cds))])

               :dc-2-quotient (fn [dcs cds]
                                [:dc-2-quotient (db/create-entities (db/system-part)
                                                                    :kv/rel-dc-day :kv/quotient-aet-pet
                                                                    (interleave dcs cds))])

               :dc-2-effectivity (fn [dcs cds]
                                   [:effectivity (first cds)])

               :dito (fn [& args]
                       [:dito args])

               :crop-block (fn [& crop-data]
                             (flatten
                              (for [[k data-map] crop-data
                                    :when (not (#{:dito :effectivity} k))]
                                (if (= k :crop)
                                  (let [cd (into {} crop-data)]
                                    (assoc data-map
                                      :crop/dc-to-rel-dc-days (map :db/id (:dc-2-rel-day cd))
                                      :crop/dc-to-developmental-state-names (map :db/id (:dc-2-name cd))
                                      :crop/rel-dc-day-to-cover-degrees (map :db/id (:dc-2-coverdegree cd))
                                      :crop/rel-dc-day-to-extraction-depths (map :db/id (:dc-2-extraction-depth cd))
                                      :crop/rel-dc-day-to-transpiration-factors (map :db/id (:dc-2-transpiration-factor cd))
                                      :crop/rel-dc-day-to-quotient-aet-pets (map :db/id (:dc-2-quotient cd))
                                      :crop/effectivity-quotient (:effectivity cd)))
                                  data-map))))}]
    (->> block
         (insta/transform trans ,,,)
         (into [] ,,,))))

(def cultivation-types-lang-de
  {1 "Hauptfrucht oder Normalpflanzung bei gartenbaulichen Fruchtarten (Feldgemüse-früh)"
   2 "Hauptfrucht als Deckfrucht oder Normalsaat bei gartenbaulichen Fruchtarten (Feldgemüse)"
   3 "Stoppelfrucht oder Direktverfahren Stecklinge (gilt nur für Ansaatjahr)"
   4 "Stoppelfrucht als Deckfrucht oder bei Grasland mehr als 12 Stunden Beweidung"
   5 "Winterzwischenfrucht oder bei mehrjährigen Futterpflanzen Umbruch nach dem 1. Schnitt"
   6 "Winterzwischenfrucht als Deckfrucht oder bei Grasland 8 - 12 Stunden Beweidung (Gemüse spät-pf)"
   7 "Zweitfrucht bzw. Sommerblanksaat bei mehrjährigen Futterpflanzen im Ansaatjahr (Gemüse spät-dr)"
   8 "Zweitfrucht als Deckfrucht bzw. Einsaat in Hauptfrucht bei mehrjährigen Futterpflanzen im Ansaatjahr oder
   bei Grasland 6 - 8 Stunden Beweidung"})

(def usage-lang-de
  {0 "ohne Angabe eines speziellen Verwendungszweckes sowie Gründüngung"
   1 "industrielle Verwertung der Ernteprodukte (z.B. Braugerste, Stärke- bzw. Zuckergewinn)"
   2 "Verwendung der Ernteprodukte bei Körnerfrüchten für Futter- und Backzwecke mit erhöhtem Rohproteingehalt"
   3 "Ganzpflanzenernte"
   4 "technische Trocknung von Grünfutterpflanzen, Kartoffeln und Rüben"
   5 "Heubereitung bei mehrschnittigen Futterpflanzen, Frischverfütterung oder Beweidung;
   Frischverfütterung von Futterkartoffeln"
   6 "Silierung von Futterpflanzen"
   7 "Sofortverzehr von Gemüse und Kartoffeln"
   8 "Lagerung von Gemüse und Kartoffeln"
   9 "Vermehrung von Gemüse und Kartoffeln"})


(comment

  (-> test-text
      crop-block-parser
      transform-crop-block
      pp/pprint)

  (def crops (slurp (str "resources/private/crops/issbruecker/BBFASTD1.TXT")))
  (def crop-blocks (cs/split crops #"\s*\*\s?-\s?\*\s?\-[^\r\n]*"))
  (pp/pprint (take 2 crop-blocks)))



(defn parse-and-transform-crop-files
  [crop-files]
  (->> (for [cf crop-files
             crop-block (-> cf
                            cjio/resource
                            slurp
                            (cs/split ,,, #"\s*\*\s?-\s?\*\s?\-[^\r\n]*")
                            (#(filter (fn [b] (-> b cs/trim empty? not)) %) ,,,))]
         (-> crop-block
             crop-block-parser
             transform-crop-block))
       (into [] ,,,)
       flatten))


(defn import-bbfastdx-crop-files-into-datomic
  [db-connection]
  (let [cfs (for [i (range 1 (inc 5))]
              (str "private/crops/issbruecker/BBFASTD" i ".txt"))
        transaction-data (parse-and-transform-crop-files cfs)]
    #_transaction-data
    (try
      @(d/transact db-connection transaction-data)
      (catch Exception e
        (log/info "Couldn't transact weather data to datomic! data: [\n" transaction-data "\n]")
        (throw e)))))



(comment "import files"

  (def x (import-bbfastdx-crop-files-into-datomic (db/connection)))
  (first x)
  (d/transact (db/connection) (first x))

  (import-bbfastdx-crop-files-into-datomic (db/connection))

  (->> (parse-crop-files)
     #_(filter #(-> % second map?) ,,,)
       pp/pprint))











