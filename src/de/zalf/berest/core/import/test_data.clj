(ns de.zalf.berest.core.import.test-data
	(:require [datomic.api :as d]
            [de.zalf.berest.core.datomic :as db]
            [de.zalf.berest.core.util :as bu]))

;add 0110/1/0 WR data
#_(defn add-winter-rye [datomic-connection]
  (let [dc-to-day (db/create-entities :system
                                      :kv/dc :kv/rel-dc-day
                                      [21 60, 31 110, 51 140, 61 155, 75 170, 92 200])
        dc-to-name (db/create-entities :system
                                       :kv/dc :kv/name
                                       [21 "Best.-beginn", 31 "Schossbeginn", 51 "Aehrenschieben", 61 "Bluete",
                                        75 "Milchreife", 92 "Todreife"])
        rel-day-to-cover-degree (db/create-entities :system
                                                    :kv/rel-dc-day :kv/cover-degree
                                                    [90 1.0])
        rel-day-to-extraction-depth (db/create-entities :system
                                                        :kv/rel-dc-day :kv/extraction-depth
                                                        [90 60, 120 90, 150 110, 170 130])
        rel-day-to-transpiration-factor (db/create-entities :system
                                                            :kv/rel-dc-day :kv/transpiration-factor
                                                            [100 1.0, 110 1.3, 190 1.3, 200 1.0, 210 0.1])
        rel-day-to-quotient (db/create-entities :system
                                                :kv/rel-dc-day :kv/quotient-aet-pet
                                                [80 0.0, 90 0.2, 110 0.8, 170 0.8, 180 0.6, 200 0.0])

        crop {:db/id (db/new-entity-id :system)
              :crop/id "0110/1/0"
              :crop/number 110
              :crop/cultivation-type 1
              :crop/usage 0
              :crop/name "Winterroggen/EJ"
              :crop/symbol "WR"
              :crop/dc-to-rel-dc-days (db/get-entity-ids dc-to-day)
              :crop/dc-to-developmental-state-names (db/get-entity-ids dc-to-name)
              :crop/rel-dc-day-to-cover-degrees (db/get-entity-ids rel-day-to-cover-degree)
              :crop/rel-dc-day-to-extraction-depths (db/get-entity-ids rel-day-to-extraction-depth)
              :crop/rel-dc-day-to-transpiration-factors (db/get-entity-ids rel-day-to-transpiration-factor)
              :crop/rel-dc-day-to-quotient-aet-pets (db/get-entity-ids rel-day-to-quotient)
              :crop/effectivity-quotient 0.17}]

    (d/transact datomic-connection
                ;print
                (flatten [dc-to-day
                          dc-to-name
                          rel-day-to-cover-degree
                          rel-day-to-extraction-depth
                          rel-day-to-transpiration-factor
                          rel-day-to-quotient
                          crop]))))


;add 0120/1/0 WG data
#_(defn add-winter-barley [datomic-connection]
  (let [dc-to-day (db/create-entities :system
                                      :kv/dc :kv/rel-dc-day
                                      [21 60, 31 120, 51 140, 61 148, 75 160, 92 185])
        dc-to-name (db/create-entities :system
                                       :kv/dc :kv/name
                                       [21 "Best.-beginn", 31 "Schossbeginn", 51 "Aehrenschieben", 61 "Bluete",
                                        75 "Milchreife", 92 "Todreife"])
        rel-day-to-cover-degree (db/create-entities :system
                                                    :kv/rel-dc-day :kv/cover-degree
                                                    [90 1.0])
        rel-day-to-extraction-depth (db/create-entities :system
                                                        :kv/rel-dc-day :kv/extraction-depth
                                                        [90 60, 120 80, 150 90, 170 110])
        rel-day-to-transpiration-factor (db/create-entities :system
                                                            :kv/rel-dc-day :kv/transpiration-factor
                                                            [110 1.0, 120 1.3, 170 1.3, 180 1.0, 200 0.1])
        rel-day-to-quotient (db/create-entities :system
                                                :kv/rel-dc-day :kv/quotient-aet-pet
                                                [80 0.0, 90 0.2, 120 0.8, 160 0.8, 175 0.6, 190 0.0])

        crop {:db/id (db/new-entity-id :system)
              :crop/id "0120/1/0"
              :crop/number 120
              :crop/cultivation-type 1
              :crop/usage 0
              :crop/name "Wintergerste/EJ"
              :crop/symbol "WG"
              :crop/dc-to-rel-dc-days (db/get-entity-ids dc-to-day)
              :crop/dc-to-developmental-state-names (db/get-entity-ids dc-to-name)
              :crop/rel-dc-day-to-cover-degrees (db/get-entity-ids rel-day-to-cover-degree)
              :crop/rel-dc-day-to-extraction-depths (db/get-entity-ids rel-day-to-extraction-depth)
              :crop/rel-dc-day-to-transpiration-factors (db/get-entity-ids rel-day-to-transpiration-factor)
              :crop/rel-dc-day-to-quotient-aet-pets (db/get-entity-ids rel-day-to-quotient)
              :crop/effectivity-quotient 0.17}]

    (d/transact datomic-connection
                ;print
                (flatten [dc-to-day
                          dc-to-name
                          rel-day-to-cover-degree
                          rel-day-to-extraction-depth
                          rel-day-to-transpiration-factor
                          rel-day-to-quotient
                          crop]))))


;add 0801/1/0 ZR-ZU data
#_(defn add-sugarbeet [datomic-connection]
  (let [dc-to-day (db/create-entities :system
                                      :kv/dc :kv/rel-dc-day
                                      [1 1, 11 20, 24 50, 32 60, 41 80, 43 100, 45 130, 47 140, 49 180])
        dc-to-name (db/create-entities :system
                                       :kv/dc :kv/name
                                       [1 "Aussaat", 11 "Aufgang", 24 "4-Blattstadium", 32 "8-Blattstadium",
                                        41 "Best.-schluss", 43 "3 Wo n. DC 41", 45 "7 Wo n. DC 41", 47 "11 Wo n.DC 41", 49 "15 Wo n.DC 41"])
        rel-day-to-cover-degree (db/create-entities :system
                                                    :kv/rel-dc-day :kv/cover-degree
                                                    [20 0.0, 40 0.2, 80 1.0])
        rel-day-to-extraction-depth (db/create-entities :system
                                                        :kv/rel-dc-day :kv/extraction-depth
                                                        [20 20, 60 50, 100 110])
        rel-day-to-transpiration-factor (db/create-entities :system
                                                            :kv/rel-dc-day :kv/transpiration-factor
                                                            [70 1.0, 80 1.2, 140 1.4, 170 1.0])
        rel-day-to-quotient (db/create-entities :system
                                                :kv/rel-dc-day :kv/quotient-aet-pet
                                                [60 0.0, 80 0.7, 90 0.8, 160 0.8, 170 0.7, 190 0.0])

        crop {:db/id (db/new-entity-id :system)
              :crop/id "0801/1/0"
              :crop/number 801
              :crop/cultivation-type 1
              :crop/usage 0
              :crop/name "Zuckerrueben"
              :crop/symbol "ZR-ZU"
              :crop/dc-to-rel-dc-days (db/get-entity-ids dc-to-day)
              :crop/dc-to-developmental-state-names (db/get-entity-ids dc-to-name)
              :crop/rel-dc-day-to-cover-degrees (db/get-entity-ids rel-day-to-cover-degree)
              :crop/rel-dc-day-to-extraction-depths (db/get-entity-ids rel-day-to-extraction-depth)
              :crop/rel-dc-day-to-transpiration-factors (db/get-entity-ids rel-day-to-transpiration-factor)
              :crop/rel-dc-day-to-quotient-aet-pets (db/get-entity-ids rel-day-to-quotient)
              :crop/effectivity-quotient 0.1}]

    (d/transact datomic-connection
                ;print
                (flatten [dc-to-day
                          dc-to-name
                          rel-day-to-cover-degree
                          rel-day-to-extraction-depth
                          rel-day-to-transpiration-factor
                          rel-day-to-quotient
                          crop]))))

;add 1030/1/0 F-MAIS data
#_(defn add-maize [datomic-connection]
  (let [dc-to-day (db/create-entities :system
                                      :kv/dc :kv/rel-dc-day
                                      [20 1, 30 15, 40 25, 50 50, 60 70, 70 80, 80 120, 90 150])
        dc-to-name (db/create-entities :system
                                       :kv/dc :kv/name
                                       [20 "Aussaat", 30 "Aufgang", 40 "4-Blattstadium", 50 "8-Blattstadium",
                                        60 "Fahnenschieben", 70 "Bluete", 80 "Milchreife", 90 "Erntereife"])
        rel-day-to-cover-degree (db/create-entities :system
                                                    :kv/rel-dc-day :kv/cover-degree
                                                    [15 0.0, 80 1.0])
        rel-day-to-extraction-depth (db/create-entities :system
                                                        :kv/rel-dc-day :kv/extraction-depth
                                                        [15 20, 80 90, 90 100])
        rel-day-to-transpiration-factor (db/create-entities :system
                                                            :kv/rel-dc-day :kv/transpiration-factor
                                                            [60 1.0, 85 1.2, 140 1.2, 170 0.1])
        rel-day-to-quotient (db/create-entities :system
                                                :kv/rel-dc-day :kv/quotient-aet-pet
                                                [40 0.0, 60 0.6, 90 0.9, 115 0.9, 140 0.7, 150 0.0])

        crop {:db/id (db/new-entity-id :system)
              :crop/id "1030/1/0"
              :crop/number 1030
              :crop/cultivation-type 1
              :crop/usage 0
              :crop/name "Mais Gr+S"
              :crop/symbol "F-MAIS"
              :crop/dc-to-rel-dc-days (db/get-entity-ids dc-to-day)
              :crop/dc-to-developmental-state-names (db/get-entity-ids dc-to-name)
              :crop/rel-dc-day-to-cover-degrees (db/get-entity-ids rel-day-to-cover-degree)
              :crop/rel-dc-day-to-extraction-depths (db/get-entity-ids rel-day-to-extraction-depth)
              :crop/rel-dc-day-to-transpiration-factors (db/get-entity-ids rel-day-to-transpiration-factor)
              :crop/rel-dc-day-to-quotient-aet-pets (db/get-entity-ids rel-day-to-quotient)
              :crop/effectivity-quotient 0.13}]

    (d/transact datomic-connection
                ;print
                (flatten [dc-to-day
                          dc-to-name
                          rel-day-to-cover-degree
                          rel-day-to-extraction-depth
                          rel-day-to-transpiration-factor
                          rel-day-to-quotient
                          crop]))))



;add 0703/1/0 KART3 data
#_(defn add-potato [datomic-connection]
  (let [dc-to-day (db/create-entities :system
                                      :kv/dc :kv/rel-dc-day
                                      [1 1, 5 35, 10 50, 15 70, 17 85, 28 115, 31 125])
        dc-to-name (db/create-entities :system
                                       :kv/dc :kv/name
                                       [1 "Pflanzung", 5 "Auflaufen", 10 "Sichtbarwerden der 1. Knospen", 15 "Blühbeginn",
                                        17 "Blühende", 28 "Braunreife", 31 "Krautabtötung Erntebeginn"])
        rel-day-to-cover-degree (db/create-entities :system
                                                    :kv/rel-dc-day :kv/cover-degree
                                                    [35 0.0, 65 1.0, 120 1.0, 135 0.0])
        rel-day-to-extraction-depth (db/create-entities :system
                                                        :kv/rel-dc-day :kv/extraction-depth
                                                        [35 20, 50 50, 60 70])
        rel-day-to-transpiration-factor (db/create-entities :system
                                                            :kv/rel-dc-day :kv/transpiration-factor
                                                            [50 1.0, 70 1.2, 115 1.2, 120 1.0])
        rel-day-to-quotient (db/create-entities :system
                                                :kv/rel-dc-day :kv/quotient-aet-pet
                                                [35 0.0, 50 0.2, 55 0.7, 85 0.8, 105 0.8, 110 0.7, 115 0.0])

        crop {:db/id (db/new-entity-id :system)
              :crop/id "0703/1/0"
              :crop/number 703
              :crop/cultivation-type 1
              :crop/usage 0
              :crop/name "Kartoffeln"
              :crop/symbol "KART3"
              :crop/dc-to-rel-dc-days (db/get-entity-ids dc-to-day)
              :crop/dc-to-developmental-state-names (db/get-entity-ids dc-to-name)
              :crop/rel-dc-day-to-cover-degrees (db/get-entity-ids rel-day-to-cover-degree)
              :crop/rel-dc-day-to-extraction-depths (db/get-entity-ids rel-day-to-extraction-depth)
              :crop/rel-dc-day-to-transpiration-factors (db/get-entity-ids rel-day-to-transpiration-factor)
              :crop/rel-dc-day-to-quotient-aet-pets (db/get-entity-ids rel-day-to-quotient)
              :crop/effectivity-quotient 0.1}]

    (d/transact datomic-connection
                ;print
                (flatten [dc-to-day
                          dc-to-name
                          rel-day-to-cover-degree
                          rel-day-to-extraction-depth
                          rel-day-to-transpiration-factor
                          rel-day-to-quotient
                          crop]))))

;add 0101/1/0 WW data
#_(defn add-winter-wheat [datomic-connection]
  (let [dc-to-day (db/create-entities :system
                                      :kv/dc :kv/rel-dc-day
                                      [21 60, 31 128, 51 158, 61 165, 75 180, 92 215])
        dc-to-name (db/create-entities :system
                                       :kv/dc :kv/name
                                       [21 "Bestockungsbeginn", 31 "Schossbeginn", 51 "Ährenschieben", 61 "Blüte",
                                        75 "Milchreife", 92 "Todreife"])
        rel-day-to-cover-degree (db/create-entities :system
                                                    :kv/rel-dc-day :kv/cover-degree
                                                    [30 0.6, 115 0.8, 125 1.0])
        rel-day-to-extraction-depth (db/create-entities :system
                                                        :kv/rel-dc-day :kv/extraction-depth
                                                        [90 60, 120 80, 150 120, 180 180])
        rel-day-to-transpiration-factor (db/create-entities :system
                                                            :kv/rel-dc-day :kv/transpiration-factor
                                                            [118 1.0, 128 1.2, 180 1.6, 210 1.0, 220 0.5])
        rel-day-to-quotient (db/create-entities :system
                                                :kv/rel-dc-day :kv/quotient-aet-pet
                                                [75 0.0, 90 0.2, 128 0.8, 180 0.8, 200 0.6, 210 0.0])

        crop {:db/id (db/new-entity-id :system)
              :crop/id "0101/1/0"
              :crop/number 101
              :crop/cultivation-type 1
              :crop/usage 0
              :crop/name "Winterweizen/EJ"
              :crop/symbol "WW"
              :crop/dc-to-rel-dc-days (db/get-entity-ids dc-to-day)
              :crop/dc-to-developmental-state-names (db/get-entity-ids dc-to-name)
              :crop/rel-dc-day-to-cover-degrees (db/get-entity-ids rel-day-to-cover-degree)
              :crop/rel-dc-day-to-extraction-depths (db/get-entity-ids rel-day-to-extraction-depth)
              :crop/rel-dc-day-to-transpiration-factors (db/get-entity-ids rel-day-to-transpiration-factor)
              :crop/rel-dc-day-to-quotient-aet-pets (db/get-entity-ids rel-day-to-quotient)
              :crop/effectivity-quotient 0.17}]

    (d/transact datomic-connection
                ;print
                (flatten [dc-to-day
                          dc-to-name
                          rel-day-to-cover-degree
                          rel-day-to-extraction-depth
                          rel-day-to-transpiration-factor
                          rel-day-to-quotient
                          crop]))))

;add 0000/1/0 Brache data
#_(defn add-fallow [datomic-connection]
  (let [dc-to-day (db/create-entities :system
                                      :kv/dc :kv/rel-dc-day
                                      [1 1])
        dc-to-name (db/create-entities :system
                                       :kv/dc :kv/name
                                       [1 "Brache"])
        rel-day-to-cover-degree (db/create-entities :system
                                                    :kv/rel-dc-day :kv/cover-degree
                                                    [1 0.0])
        rel-day-to-extraction-depth (db/create-entities :system
                                                        :kv/rel-dc-day :kv/extraction-depth
                                                        [1 1])
        rel-day-to-transpiration-factor (db/create-entities :system
                                                            :kv/rel-dc-day :kv/transpiration-factor
                                                            [1 1.0])
        rel-day-to-quotient (db/create-entities :system
                                                :kv/rel-dc-day :kv/quotient-aet-pet
                                                [1 0.0])

        fallow {:db/id (db/new-entity-id :system)
                :crop/id "0000/1/0"
                :crop/number 0
                :crop/cultivation-type 1
                :crop/usage 0
                :crop/name "Brache"
                :crop/symbol "BRACHE"
                :crop/dc-to-rel-dc-days (db/get-entity-ids dc-to-day)
                :crop/dc-to-developmental-state-names (db/get-entity-ids dc-to-name)
                :crop/rel-dc-day-to-cover-degrees (db/get-entity-ids rel-day-to-cover-degree)
                :crop/rel-dc-day-to-extraction-depths (db/get-entity-ids rel-day-to-extraction-depth)
                :crop/rel-dc-day-to-transpiration-factors (db/get-entity-ids rel-day-to-transpiration-factor)
                :crop/rel-dc-day-to-quotient-aet-pets (db/get-entity-ids rel-day-to-quotient)
                :crop/effectivity-quotient 0.0}]

    (d/transact datomic-connection
                ;print
                (flatten [dc-to-day
                          dc-to-name
                          rel-day-to-cover-degree
                          rel-day-to-extraction-depth
                          rel-day-to-transpiration-factor
                          rel-day-to-quotient
                          fallow]))))

(defn add-zalf-1993
  [in-partition plot-e-id]
  (let [year 1993
        technology {:db/id (db/new-entity-id in-partition)
                    :technology/type :technology.type/sprinkler
                    :technology/sprinkle-loss-factor 0.2
                    :technology/cycle-days 1
                    :donation/min 5.0
                    :donation/max 30.0
                    :donation/opt 20.0
                    :donation/step-size 5.0}

        dc-assertions (db/create-dc-assertions in-partition year
                                               [[[21 5] 1]
                                                [[1 6] 11]
                                                [[6 10] 49]])
        irrigation-donations (db/create-irrigation-donations in-partition year
                                                             [[[13 5] 6.0]
                                                              [[18 5] 6.0]
                                                              [[21 5] 6.0]
                                                              [[25 5] 2.0]
                                                              [[26 8] 15.0]
                                                              [[27 8] 15.0]])
        crop-instances [{:db/id (db/new-entity-id in-partition)
                         :crop.instance/template [:crop/id "0801-1-0"]
                         :crop.instance/name "Zuckerrübe - 801/1/0"
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
               :plot.annual/donations (map :db/id irrigation-donations)
               }
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

(defn add-zalf-1994
  [in-partition plot-e-id]
  (let [year 1994
        technology {:db/id (db/new-entity-id in-partition)
                    :technology/type :technology.type/sprinkler
                    :technology/sprinkle-loss-factor 0.2
                    :technology/cycle-days 1
                    :donation/min 5.0
                    :donation/max 30.0
                    :donation/opt 20.0
                    :donation/step-size 5.0}

        dc-assertions (db/create-dc-assertions in-partition year
                                               [[[19 4] 31]
                                                [[26 5] 45]
                                                [[14 6] 65]
                                                [[26 7] 92]])
        irrigation-donations (db/create-irrigation-donations in-partition year
                                                             [[[16 5] 12.0]
                                                              [[24 6] 10.0]
                                                              [[28 6] 10.0]
                                                              [[29 6] 11.0]
                                                              [[1 7] 10.0]
                                                              [[5 7] 11.0]])
        crop-instances [{:db/id (db/new-entity-id in-partition)
                         :crop.instance/template [:crop/id "0101-1-0"]
                         :crop.instance/name "Winterweizen/EJ - 0101/1/0"
                         :crop.instance/dc-assertions (map :db/id dc-assertions)}]
        initial-sms (db/create-entities in-partition
                                        :soil/upper-boundary-depth :soil/soil-moisture
                                        [30 60.74, 60 57.04, 90 50.38])
        plot* {:db/id (db/new-entity-id in-partition)
               :plot.annual/year year
               :plot.annual/abs-day-of-initial-soil-moisture-measurement (bu/date-to-doy 8 3 year)
               :plot.annual/initial-soil-moistures (map :db/id initial-sms)
               :plot.annual/initial-sm-unit :soil-moisture.unit/mm
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

(defn add-zalf-1995
  [in-partition plot-e-id]
  (let [year 1995
        technology {:db/id (db/new-entity-id in-partition)
                    :technology/type :technology.type/sprinkler
                    :technology/sprinkle-loss-factor 0.2
                    :technology/cycle-days 1
                    :donation/min 5.0
                    :donation/max 30.0
                    :donation/opt 20.0
                    :donation/step-size 5.0}

        dc-assertions (db/create-dc-assertions in-partition year
                                               [[[24 4] 31]
                                                [[7 6] 61]
                                                [[17 7] 92]])
        irrigation-donations (db/create-irrigation-donations in-partition year
                                                             [[[30 6] 10.0]])
        crop-instances [{:db/id (db/new-entity-id in-partition)
                         :crop.instance/template [:crop/id "0120-1-0"]
                         :crop.instance/name "Wintergerste/EJ - 0120/1/0"
                         :crop.instance/dc-assertions (map :db/id dc-assertions)}]
        initial-sms (db/create-entities in-partition
                                        :soil/upper-boundary-depth :soil/soil-moisture
                                        [30 42.94, 60 37.64, 90 43.91])
        plot* {:db/id (db/new-entity-id in-partition)
               :plot.annual/year year
               :plot.annual/abs-day-of-initial-soil-moisture-measurement (bu/date-to-doy 13 3 year)
               :plot.annual/initial-soil-moistures (map :db/id initial-sms)
               :plot.annual/initial-sm-unit :soil-moisture.unit/mm
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

(defn add-zalf-1996
  [in-partition plot-e-id]
  (let [year 1996
        technology {:db/id (db/new-entity-id in-partition)
                    :technology/type :technology.type/sprinkler
                    :technology/sprinkle-loss-factor 0.2
                    :technology/cycle-days 1
                    :donation/min 5.0
                    :donation/max 30.0
                    :donation/opt 20.0
                    :donation/step-size 5.0}

        dc-assertions (db/create-dc-assertions in-partition year
                                               [[[10 4] 26]
                                                [[6 5] 31]
                                                [[30 5] 61]
                                                [[31 8] 92]])
        irrigation-donations (db/create-irrigation-donations in-partition year
                                                             [[[20 6] 10.0]
                                                              [[25 6] 10.0]
                                                              [[27 6] 10.0]])
        crop-instances [{:db/id (db/new-entity-id in-partition)
                         :crop.instance/template [:crop/id "0110-1-0"]
                         :crop.instance/name "Winterroggen/EJ - 0110/1/0"
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

(defn add-zalf-1997
  [in-partition plot-e-id]
  (let [year 1997
        technology {:db/id (db/new-entity-id in-partition)
                    :technology/type :technology.type/sprinkler
                    :technology/sprinkle-loss-factor 0.2
                    :technology/cycle-days 1
                    :donation/min 5.0
                    :donation/max 30.0
                    :donation/opt 20.0
                    :donation/step-size 5.0}

        dc-assertions (db/create-dc-assertions in-partition year
                                               [[[3 4] 1]
                                                [[5 5] 10]
                                                [[23 9] 49]])
        irrigation-donations (db/create-irrigation-donations in-partition year
                                                             [[[20 6] 12.5]
                                                              [[3 7] 20.0]
                                                              [[10 7] 10.0]
                                                              [[11 7] 10.0]
                                                              [[17 7] 25.0]
                                                              [[8 8] 25.0]
                                                              [[12 8] 25.0]
                                                              [[16 8] 30.0]
                                                              [[19 8] 27.5]
                                                              [[22 8] 40.0]
                                                              [[28 8] 25.0]])
        crop-instances [{:db/id (db/new-entity-id in-partition)
                         :crop.instance/template [:crop/id "0801-1-0"]
                         :crop.instance/name "Zuckerrübe - 801/1/0"
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

(defn add-zalf-1998
  [in-partition plot-e-id]
  (let [year 1998
        technology {:db/id (db/new-entity-id in-partition)
                    :technology/type :technology.type/sprinkler
                    :technology/sprinkle-loss-factor 0.2
                    :technology/cycle-days 1
                    :donation/min 5.0
                    :donation/max 30.0
                    :donation/opt 20.0
                    :donation/step-size 5.0}

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

(defn add-zalf-test-farm [in-partition]
(let [new-farm-e-id (db/new-entity-id in-partition)]
  @(d/transact (db/connection)
               [{:db/id new-farm-e-id
                 :farm/id (d/squuid)
                 :farm/user-id "zalf_test-farm"
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
(defn add-zalf-test-plot
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
              :plot/stt [:soil.stt/symbol "D2 a"] #_[:soil.stt/key 6212]
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

(comment
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
