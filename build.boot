(boot.core/set-env!
 :resource-paths #{"src"}
 :dependencies '[[org.clojure/clojure "1.9.0-alpha14"]
                 ;[boot/aether "2.6.0" :exclusions [org.clojure/clojure]]
                 [org.apache.maven/maven-aether-provider "3.3.9"]
                 ;; [org.eclipse.aether/aether-api "1.1.0"]
                 ;; [org.eclipse.aether/aether-spi "1.1.0"]
                 ;; [org.eclipse.aether/aether-impl "1.1.0"]
                 [org.eclipse.aether/aether-transport-file "1.1.0"]
                 [org.eclipse.aether/aether-transport-wagon "1.1.0"]
                 [org.eclipse.aether/aether-connector-basic "1.1.0"]
                 [org.apache.maven.wagon/wagon-provider-api "2.10"]
                 [org.apache.maven.wagon/wagon-http "2.10"]
                 [org.apache.maven.wagon/wagon-ssh "2.10"]])
