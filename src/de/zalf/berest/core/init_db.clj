(ns de.zalf.berest.core.init-db
  (:require [datomic.api :as d]
            [clj-time.core :as date]
            [de.zalf.berest.core.datomic :as db]
            [de.zalf.berest.core.import.zalf-climate-data :as climate-import]
            [de.zalf.berest.core.import.dwd-data :as dwd]
            [de.zalf.berest.core.import.berest-90-crops :as crop-import]
            [de.zalf.berest.core.import.test-data :as test-data]
            [de.zalf.berest.core.import.berest-slope-and-stt :as stt]))

(defn delete-db
  []
  (db/delete-db! db/*db-id*))

(defn create-and-import-data
  []

  ;create initial db and schemas
  (apply db/create-db db/*db-id* db/datomic-schema-files)

  ;register some users
  (db/register-user "michael" "..." "Michael Berg" [:admin :guest :farmer :consultant])
  (db/register-user "guest" "guest" "Guest Guest")
  (db/register-user "zalf" "..." "Zalf Zalf" [:consultant])

  ;add soil data
  (stt/import-berest-slope-and-stt-data (db/connection))

  ;add climate data
  ;local zalf climate data
  (climate-import/transact-zalf-data (db/connection))
  ;dwd data for berest field trial
  (dwd/bulk-import-dwd-data-into-datomic (date/date-time 2014 2 3)
                                         (date/date-time 2014 4 17))
  (dwd/set-import-time-settings 11 0)

  ;add crop data
  (crop-import/import-bbfastdx-crop-files-into-datomic (db/connection))

  ;add some test data
  (test-data/add-zalf-test-farm :zalf)
  (test-data/add-zalf-test-plot :zalf)


  )





