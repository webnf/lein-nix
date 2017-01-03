(defproject lein-nix "0.0.1"
  :description "Generate nix expressions from leiningen
  - classpath derivations
  - launchers for leiningen tasks"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                 [org.apache.maven/maven-aether-provider "3.3.9"]
                 [org.eclipse.aether/aether-transport-file "1.1.0"]
                 [org.eclipse.aether/aether-transport-wagon "1.1.0"]
                 [org.eclipse.aether/aether-connector-basic "1.1.0"]
                 [org.apache.maven.wagon/wagon-provider-api "2.10"]
                 [org.apache.maven.wagon/wagon-http "2.10"]
                 [org.apache.maven.wagon/wagon-ssh "2.10"]]
  ;:eval-in-leiningen true
  )
