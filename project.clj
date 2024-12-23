(defproject fprog_project "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/core.match "1.1.0"]
                 [com.clojure-goes-fast/clj-async-profiler "1.4.0"]]
  :main ^:skip-aot fprog-project.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"
                                  "-Dcom.sun.management.jmxremote"
                                  "-Dcom.sun.management.jmxremote.ssl=false"
                                  "-Dcom.sun.management.jmxremote.authenticate=false"
                                  "-Dcom.sun.management.jmxremote.port=43210"
                                  "-Djdk.attach.allowAttachSelf"
                                  ]}})
