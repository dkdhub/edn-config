(defproject com.dkdhub/edn-config "1.0.0"
  :description "A small library for explicit, intentful configuration."
  :url "http://github.com/dkdhub/edn-config"
  :license {:name "The MIT License"
            :url  "http://opensource.org/licenses/MIT"}

  :eastwood {:namespaces [edn-config.core edn-config.alpha.core]}
  :profiles
  {:provided {:dependencies [[org.clojure/clojure "1.11.1"]]}
   :dev      {:plugins [[lein-cljfmt "0.5.7"]
                        [jonase/eastwood "0.3.4"]]}})
