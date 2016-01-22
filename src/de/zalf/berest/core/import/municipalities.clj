(ns de.zalf.berest.core.import.municipalities
	(:require [datomic.api :as d]
            [clj-time.core :as ctc]
            [de.zalf.berest.core.datomic :as db]
            [de.zalf.berest.core.core :as bc]
            [de.zalf.berest.core.util :as bu]
            [de.zalf.berest.core.helper :as bh]))

(def countries
  {:de "Bundesrepublik Deutschland"
   :fi "Finland"})

(def german-states
  {:bw "Baden-Württemberg"
   :by "Bayern"
   :be "Berlin"
   :bb "Brandenburg"
   :hb "Bremen"
   :hh "Hamburg"
   :he "Hessen"
   :mv "Mecklenburg-Vorpommern"
   :ni "Niedersachsen"
   :nw "Nordrhein-Westfalen"
   :rp "Rheinland-Pfalz"
   :sl "Saarland"
   :sn "Sachsen"
   :st "Sachsen-Anhalt"
   :sh "Schleswig-Holstein"
   :th "Thüringen"})

(def municipalities
  {:mol "Märkisch-Oderland"
   :los "Oder-Spree"
   :lds "Dahme-Spreewald"})


#_(defn add-zalf-1998
  [in-partition plot-e-id]
  (let [year 1998
        technology {:db/id (db/new-entity-id in-partition)
                    :technology/type :technology.type/sprinkler
                    :technology/sprinkle-loss-factor 0.2
                    :technology/cycle-days 1
                    :donation/min 5
                    :donation/max 30
                    :donation/opt 20
                    :donation/step-size 5}

        dc-assertions (db/create-dc-assertions in-partition year
                                               [[[25 5] 45]
                                                [[9 6] 65]
                                                [[27 7] 92]])
        irrigation-donations (db/create-irrigation-donations in-partition year
                                                             [[[19 5] 20.0]
                                                              [[28 5] 22.0]
                                                              [[4 6] 20.0]
                                                              [[24 6] 20.0]
                                                              [[3 7] 20.0]])
        crop-instances [{:db/id (db/new-entity-id in-partition)
                         :crop.instance/template [:crop/id "0101-1-0"]
                         :crop.instance/name "Winterweizen/EJ - 0101/1/0"
                         :crop.instance/dc-assertions (map :db/id dc-assertions)}]
        initial-sms (db/create-entities in-partition
                                        :soil/upper-boundary-depth :soil/soil-moisture
                                        [30 80.0, 60 90.0, 90 100.0, 150 100.0])
        plot* {:db/id (db/new-entity-id in-partition)
               :plot.annual/year year
               :plot.annual/abs-day-of-initial-soil-moisture-measurement (bu/date-to-doy 31 3 year)
               :plot.annual/initial-soil-moistures (map :db/id initial-sms)
               :plot.annual/initial-sm-unit :soil-moisture.unit/pFK
               :plot.annual/technology (:db/id technology)
               :plot.annual/crop-instances (map :db/id crop-instances)
               :plot.annual/donations (map :db/id irrigation-donations)}
        plot {:db/id plot-e-id
              :plot/annuals (:db/id plot*)}]
    (try
      @(d/transact (db/connection) (flatten [technology
                                             dc-assertions
                                             irrigation-donations
                                             crop-instances
                                             initial-sms
                                             plot*
                                             plot]))
      (catch Exception e
        (println "Couldn't transact year " year " Exception: " e)))))

#_(defn add-zalf-test-farm [in-partition user-id]
(let [new-farm-e-id (db/new-entity-id in-partition)]
  @(d/transact (db/connection)
               [{:db/id new-farm-e-id
                 :farm/id "zalf_test-farm"
                 :farm/name "ZALF Test Betrieb"
                 :user/_farms new-farm-e-id
                 :farm/addresses [{:address/street "Eberswalder Str. 84"
                                   :address/postal-code "15374"
                                   :address/city "Müncheberg"
                                   :address/municipality "Märkisch-Oderland"
                                   :address/municipality-short "MOL"
                                   :address/state "Brandenburg"
                                   :address/state-short "BRA"
                                   :address/country "Deutschland"
                                   :address/country-short "GER"}]
                 :farm/authorative-weather-station [:weather-station/id "zalf_lokal"]}])))

;add plot zalf test plot
#_(defn add-zalf-test-plot
  [in-partition]
  (let [fcs (db/create-entities in-partition
                                :soil/upper-boundary-depth :soil/field-capacity
                                 [30 17.5, 60 15.5, 90 15.7, 120 15.7, 200 12.5])

        pwps (db/create-entities in-partition
                                 :soil/upper-boundary-depth :soil/permanent-wilting-point
                                 [30 3.5, 60 3.0, 90 3.7, 120 3.7, 200 3.0])

        #_ka5-sts #_(db/create-entities in-partition
                                    :soil/upper-boundary-depth :soil./ka5-soil-type
                                    [30 "Ss", 60 "Ss", 90 "Ss", 120 "Ss", 200 "Ss"])

        new-plot-e-id (db/new-entity-id in-partition)

        plot {:db/id new-plot-e-id
              :farm/_plots [:farm/id "zalf_test-farm"]
              :plot/id "zalf.test-farm_versuchsfeld"
              :plot/name "Versuchsfeld"
              :plot/crop-area 1.0
              :plot/irrigation-area 1.0
              :plot/stt 6212
              :plot/slope [:slope/key 1]
              :plot/field-capacities (map :db/id fcs)
              :plot/fc-pwp-unit :soil-moisture.unit/volP
              :plot/permanent-wilting-points (map :db/id pwps)
              ;:plot/ka5-soil-types (map :db/id ka5-sts)
              :plot/groundwaterlevel 300
              :plot/damage-compaction-depth 300
              :plot/damage-compaction-area 0.0}

        {:keys [db-after tempids] :as tx} (->> [fcs pwps #_ka5-sts plot]
                                               flatten
                                               (d/transact (db/connection) ,,,)
                                               .get)

        plot-e-id (d/resolve-tempid db-after tempids (:db/id plot))

				_ (add-zalf-1993 in-partition plot-e-id)
        _ (add-zalf-1994 in-partition plot-e-id)
        _ (add-zalf-1995 in-partition plot-e-id)
        _ (add-zalf-1996 in-partition plot-e-id)
        _ (add-zalf-1997 in-partition plot-e-id)
        _ (add-zalf-1998 in-partition plot-e-id)]
    true))

#_(comment
  "install test data"

  (add-zalf-test-farm :zalf)
  (add-zalf-test-plot :zalf)

  @(d/transact (db/connection)
               [[:db/add [:user/id "zalf"] :user/farms [:farm/id "zalf_test-farm"]]])

  )


#_(defn install-test-data [db-connection in-partition]
	#_(bh/juxt* add-sugarbeet
						add-maize
						add-potato
						add-winter-rye
						add-winter-barley
						add-winter-wheat
						add-fallow
						db-connection)
  (add-zalf-test-farm db-connection in-partition)
	(add-zalf-test-plot db-connection in-partition))
