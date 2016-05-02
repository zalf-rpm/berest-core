(defproject
  de.zalf.berest/berest-core "0.2.0"
  :description "BEREST core"
  :url ""
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]

                 [com.datomic/datomic-pro "0.9.5344" :exclusions [joda-time]]
                 [com.amazonaws/aws-java-sdk-dynamodb "1.9.39" :exclusions [joda-time]]

                 #_[buddy "0.1.0-beta4"]
                 [crypto-password "0.1.3"]

                 [ring "1.4.0"]
                 [ring-server "0.4.0"]
                 [fogus/ring-edn "0.3.0"]

                 [clj-time "0.11.0"]

                 [com.cognitect/transit-clj "0.8.281"]

                 [clojure-csv "2.0.1"]
                 [org.clojure/algo.generic "0.1.2"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [com.taoensso/timbre "3.1.6"]
                 [org.clojure/core.match "0.2.2"]
                 [com.velisco/clj-ftp "0.3.5"]
                 [instaparse "1.4.1"]
                 [org.clojure/tools.logging "0.3.1"]
                 [org.clojure/tools.namespace "0.2.11"]

                 [clojurewerkz/propertied "1.2.0"]
                 [clojurewerkz/quartzite "2.0.0"]]

  :repositories {
                 "my.datomic.com" {:url "https://my.datomic.com/repo"
                                   :username "michael.berg@zalf.de"
                                   :password "dfe713b3-62f0-469d-8ac9-07d6b02b0175"}}

  :profiles {:dev {:dependencies []
                   :source-paths []}}

  :min-lein-version "2.0.0"

  :source-paths ["src"]
  :resource-paths ["resources" #_"private-resources"])

  ;:main ^{:skip-aot true} berest.core













