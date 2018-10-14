(defproject gov.nist.mm/facility "0.2.0"
  :description "Production Facility Ontology Management System"
  :url "https://github.com/usnistgov/facility"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
   :dependencies [[org.clojure/clojure        "1.9.0"]
                  [org.immutant/immutant      "2.1.10"]
                  [compojure                  "1.6.1"]
                  [ring/ring-devel            "1.7.0"]
                  [ring/ring-core             "1.7.0"]
                  [org.clojure/core.memoize   "0.7.1"]
                  [clj-time                   "0.15.0"]
                  [cheshire                   "5.8.1"]
                  [environ                    "1.0.2"]
                  [org.clojure/java.jdbc      "0.7.8"]
                  [hiccup                     "1.0.5"]
                  [uk.org.russet/tawny-owl    "1.6.0"]
                  [org.clojure/core.logic     "0.8.11"] 
                  [edu.ucdenver.ccp/kr-jena-core "1.4.19"]] ; going away
  :plugins [[lein-immutant "2.0.0"]]
  :main ^:skip-aot gov.nist.mm.facility
  :uberjar-name "facility-standalone.jar"
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :repl-options {:init-ns gov.nist.mm.facility})

