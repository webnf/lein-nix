(ns lein-nix.test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [leiningen.core.classpath :as lc]
            [leiningen.core.project :as lp]
            [cemerick.pomegranate.aether :as aether]
            [cemerick.pomegranate :as pomegranate]
            [clojure.java.io :as io]))

(def prj1
  (-> {:dependencies
       '[[webnf/base "0.1.17"]]}
      (lp/make 'test1 "SNAP" (io/file "."))
      (lp/init-project [])))

(def prj2
  (-> {:dependencies
       '[[webnf/base "0.1.18"]]}
      (lp/make 'test1 "SNAP" (io/file "."))
      (lp/init-project [])))

(aether/dependency-hierarchy
 '[[webnf/base "0.1.17"]]
 (lc/resolve-dependencies :dependencies prj1))

(pprint
 (aether/dependency-hierarchy
  '[[webnf/base "0.1.17"]]
  (aether/resolve-dependencies
   :repositories {"clojars" "http://clojars.org/repo"
                  "central" "http://repo1.maven.org/maven2/"}
   :coordinates '[[webnf/base "0.1.17"]
                  [org.clojure/clojure "1.8.0"]
                  [webnf.deps/logback "0.1.17"]])))

(pprint
 (aether/resolve-dependencies
   :repositories {"clojars" "http://clojars.org/repo"
                  "central" "http://repo1.maven.org/maven2/"}
   :coordinates '[[webnf/base "0.1.17"]
                  [org.clojure/clojure "1.8.0"]
                  [webnf.deps/logback "0.1.17"]]))

(meta (ffirst (lc/dependency-hierarchy :dependencies prj1)))
