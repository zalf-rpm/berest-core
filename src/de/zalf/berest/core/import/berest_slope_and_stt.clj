(ns de.zalf.berest.core.import.berest-slope-and-stt
  (:require [datomic.api :as d]
            [de.zalf.berest.core.datomic :as db]
            [clojure.string :as cs]
            [clojure.tools.logging :as log]))

(defn create-slope-entity
  [[key symbol description]]
  {:db/id (db/new-entity-id :system)
   :slope/key key
   :slope/symbol symbol
   :slope/description description})

(defn import-slope-data
  [db-connection]
  (let [tx-data (mapv create-slope-entity [[1 "NFT 01" "eben"]
                                           [2 "NFT 03" "flach"]
                                           [3 "NFT 05" "flach mit mäßig geneigten Anteilen"]
                                           [4 "NFT 07" "flach mit stark geneigten Anteilen"]
                                           [5 "NFT 09" "mäßig geneigt mit stark geneigten Anteilen"]
                                           [6 "NFT 11" "stark geneigt"]])]
    #_tx-data
    (try
      @(d/transact db-connection tx-data)
      (catch Exception e
        (log/info "Couldn't transact BEREST slope data to datomic! tx-data:\n" tx-data)
        (throw e)))))

(defn create-substrate-entity
  [[key symbol description fc30 pwp30 fc150 pwp150]]
  {:db/id (db/new-entity-id :system)
   :soil.substrate/key key
   :soil.substrate/symbol symbol
   :soil.substrate/description description
   :soil.substrate/field-capacities [{:soil/upper-boundary-depth 30
                                      :soil/field-capacity fc30}
                                     {:soil/upper-boundary-depth 150
                                      :soil/field-capacity fc150}]
   :soil.substrate/permanent-wilting-points [{:soil/upper-boundary-depth 30
                                              :soil/permanent-wilting-point pwp30}
                                             {:soil/upper-boundary-depth 150
                                              :soil/permanent-wilting-point pwp150}]})

(defn import-substrate-group-data
  [db-connection]
  (let [tx-data (mapv create-substrate-entity
                      [[1 "S" "Sand" 10.0 2.5 9.0 2.0]
                       [2 "l'S" "schwach lehmiger Sand" 14.0 4.5 12.0 3.0]
                       [3 "lS" "stark lehmiger Sand" 20.0 6.0 18.0 5.0]
                       [4 "uS" "schluffiger Sand" 24.0 7.0 22.0 6.0]
                       [5 "sL" "sandiger Lehm" 26.0 8.5 25.0 8.0]
                       [6 "L" "Lehm" 30.0 11.0 28.0 9.0]
                       [7 "lU" "lehmiger Schluff" 31.2 11.6 36.4 12.6]
                       [8 "UL" "Schlufflehm" 32.7 16.1 34.4 17.4]
                       [9 "uT" "schluffiger Ton" 35.6 17.6 37.3 23.9]
                       [10 "lT" "lehmiger Ton" 35.5 20.0 36.5 24.5]
                       [11 "T" "Ton" 40.0 26.6 44.1 29.3]
                       [12 "Mo a" "Niedermoor mit beginnender Vererdung (Fen)" 78.0 20.0 78.0 20.0]
                       [13 "Mo b" "Niedermoor mit deutlicher Vererdung (Erdfen)" 70.0 20.0 70.0 20.0]
                       [14 "Mo c" "Niedermoor mit deutlicher Vermullung (Mulm)" 58.0 20.0 58.0 20.0]
                       [15 "UL(Lö)" "" 32.7 10.0 34.4 11.0]
                       [16 "uT(Lö)" "" 35.6 11.0 37.3 12.0]
                       [17 "lT(Lö)" "" 35.5 12.0 36.5 13.0]])]
    #_tx-data
    (try
      @(d/transact db-connection tx-data)
      (catch Exception e
        (log/info "Couldn't transact BEREST substrate-group data to datomic! tx-data:\n" tx-data)
        (throw e)))))

(defn create-stt-entity
  [[key symbol description substrate-group-keys]]
  {:db/id (db/new-entity-id :system)
   :soil.stt/key key
   ;:soil.stt/code code
   :soil.stt/symbol symbol
   :soil.stt/description description
   :soil.stt/substrate-groups (map #(vector :soil.substrate/key %) substrate-group-keys)})

(defn import-stt-data
  [db-connection]
  (let [tx-data (mapv create-stt-entity
                      [[1121 "Lö1 a" "Lößbestimmte Schwarzerden" [15 16 17]]
                       [1122 "Lö1 b" "Lößbestimmte Schwarzerden mit Staunässe und/oder Grundwassereinfluß" [15 16 17]]
                       [1123 "Lö1 c" "Lößtieflehm- und/oder lößkerfbestimmte Schwarzerden" [15 16 17]]
                       [1133 "Lö2 c" "Decklößbestimmte Schwarzerden" [15 16 17]]
                       [1134 "Lö2 d" "Löß- und berglehmbestimmte Schwarzerden und/oder Rendzinen" [15 16 17]]
                       [1211 "Lö3 a" "Lößbestimmte Parabraunerden und Fahlerden" [15 16 17]]
                       [1213 "Lö3 c" "Sickerwasserbestimmte und/oder staunässebeeinflusste Lößtieflehme" [15 16 17]]
                       [1222 "Lö4 b" "Staunässe- und/oder grundwasserbeeinflußte Löße" [15 16 17]]
                       [1223 "Lö4 c" "Sickerwasser- bis staunässebeeinflußte Decklöße, z.T. Lößtieflehme" [15 16 17]]
                       [1323 "Lö6 c" "Staunässe- und/oder grundwasserbestimmte Löße" [15 16 17]]
                       [1312 "Lö5 b" "Staunässe- und/oder grundwasserbestimmte Lößtieflehme, z.T. Decklöße" [15 16 17]]
                       [1313 "Lö5 c" "Staunässe- und/oder grundwasserbestimmte Löße und Berglehme" [15 16 17]]
                       [1322 "Lö6 b" "Sickerwasser- bis staunässebeeinflußte Löße und Berglehme" [15 16 17]]
                       [1411 "V1 a" "Lößbeeinflußte Lehme und Tone mit Schwarzerden" [8 9 10 11]]
                       [2211 "V2 a" "Vernässungsfreie Lehme und Berglehme aus Karbonatgestein" [8 9 10 11]]
                       [2213 "V2 c" "Vernässungsfreie Lehme und Berglehme aus Karbonatgestein mit Lößeinfluß" [8 9 10 11]]
                       [2221 "V3 a" "Vernässungsfreie Tone bis Bergton mit Berglehm" [8 9 10 11]]
                       [2222 "V3 b" "Staunässe- und/oder grundwasserbestimmte Tone bis Berglehme" [8 9 10 11]]
                       [2223 "V3 c" "Vernässungsfreie Schuttlehme und -tone aus Karbonatgestein" [8 9 10 11]]
                       [2231 "V4 a" "Vernässungsfreie Berglehmsande bis -sandlehme" [2 3 4 5 6 7 8]]
                       [2233 "V4 c" "Vernässungsfreie, lößbeeinflußte Bergsandlehme" [5 6 7 8]]
                       [2241 "V5 a" "Vernässungsfreie Bergsandlehme bis Berglehme auf Schiefergestein und/oder Sandgestein" [5 6 7 8]]
                       [2242 "V5 b" "Staunässebeeinflußte Bergsandlehme bis Berglehme auf Schiefergestein und/oder Buntsandstein, z.T. lößbeeinflußt" [5 6 7 8]]
                       [2243 "V5 c" "Schuttlehme auf Schiefer und Buntsandstein" [2 3 4 5 6 7 8]]
                       [2132 "V6 b" "Staunässe- und/oder grundwasserbestimmte Bergsandlehme bis -lehme" [5 6 7 8]]
                       [2311 "V7 a" "Vernässungsfreie Bergsandlehme bis Berglehme auf Gneis, Glimmerschiefer und/oder Granit" [5 6 7 8]]
                       [2312 "V7 b" "Staunässebeeinflußte Bergsandlehme bis Berglehme auf Gneis, z.T. lößbeeinflußt" [5 6 7 8]]
                       [2313 "V7 c" "Vernässungsfreie Schuttlehmsande bis Schuttlehme auf Gneis, Glimmerschiefer und Granit" [2 3 4 5 6 7 8]]
                       [2321 "V8/V9 a" "Bergsubstrate der Hochlagen" [2 3 4 5 6 7 8]]
                       [3211 "Al1/2 a" "Anhydromorphe Auentone, z.T. -schluffe" [9 10 11]]
                       [3212 "Al1/2 b" "Halb- und vollhydromorphe Auentone" [9 10 11]]
                       [3213 "Al1/2 c" "Halb- und vollhydromorphe Deckauentone" [9 10 11]]
                       [3311 "Al3 a" "Anhydromorphe Auenlehme und -decklehme, z.T. -schluffe" [5 6 7 8]]
                       [3312 "Al3 b" "Halb- und vollhydromorphe Auenlehme und -decklehme" [5 6 7 8]]
                       [3313 "Al3 c" "Halb- und vollhydromorphe Auenlehmsande" [2 3 4 5]]
                       [3321 "D6 a" "Sickerwasserbestimmte Lehme" [5 6 7 8]]
                       [4112 "D6 b" "Staunässe- und/oder grundwasserbestimmte Lehme und Tone" [5 6 7 8]]
                       [3323 "D6 c" "Sandlöße mit schwarzerdeähnlichen Böden" [5 6 7 8]]
                       [4211 "D5 a" "Sickerwasserbestimmte Lehme und Tieflehme" [5 6 7 8]]
                       [4212 "D5 b" "Staunässe- und/oder grundwasserbestimmte Lehme und Tieflehme" [5 6 7 8]]
                       [3413 "D5 c" "Sickerwasser- und staunässebeeinflußte Sandlöße" [5 6 7 8]]
                       [4221 "D4 a" "Sickerwasserbestimmte Tieflehme" [3 4 5 6]]
                       [4222 "D4 b" "Staunässe- und/oder grundwasserbestimmte Tieflehme" [3 4 5 6]]
                       [3423 "D4 c" "Sickerwasser- und staunässebeeinflußte Decksandlöße" [3 4 5 6]]
                       [6111 "D3 a" "Sickerwasserbestimmte Tieflehme und Sande" [2 3 4 5]]
                       [6112 "D3 b" "Grundwasser- und staunässebestimmte Sande und Tieflehme" [2 3 4 5]]
                       [5113 "D3 c" "Sickerwasserbestimmte Decklehmsande" [2 3 4 5]]
                       [6211 "D2 a" "Sickerwasserbestimmte Sande und Sande mit Tieflehm" [1]]
                       [6212 "D2 b" "Grundwasserbestimmte Sande" [1]]
                       [6311 "D1 a" "Sickerwasserbestimmte Sande" [1]]
                       [0713 "Mo1 c" "Sandunterlagerte oder sandbedeckte Moore" [11 12 13]]
                       [0722 "Mo2 b" "Tiefgründige Torfmoore" [11 12 13]]
                       [0723 "Mo2 c" "Mudde- und/oder lehmunterlagerte oder -überlagerte Moore" [11 12 13]]
                       [1511 "K1 a" "Sandige Kippsubstrate" [1 2 3 4 5]]
                       [1512 "K1 b" "Lehmige bis tonige Kippsubstrate" [5 6 7 8 9 10 11]]
                       [1513 "K1 c" "Kohlenhaltige Kippsubstrate" [1 2 3 4 5 6 7 8 9 10 11]]])]
    #_tx-data
    (try
      @(d/transact db-connection tx-data)
      (catch Exception e
        (log/info "Couldn't transact BEREST STT data to datomic! tx-data:\n" tx-data)
        (throw e)))))


(defn import-berest-slope-and-stt-data
  [db-connection]
  (try
    (import-slope-data db-connection)
    (import-substrate-group-data db-connection)
    ;substrate groups need to exist before STT data can be imported
    (import-stt-data db-connection)
    (catch Exception e
      (log/info "Couldn't transact either slope, substrate group or STT data to datomic!")
      (throw e)))
  )


(comment

  (import-berest-slope-and-stt-data (db/connection))

  )











