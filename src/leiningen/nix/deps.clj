(ns leiningen.nix.deps
  (:import
   java.nio.file.Files
   java.nio.file.attribute.FileAttribute
   (org.eclipse.aether RepositorySystem RepositorySystemSession)
   (org.eclipse.aether.resolution ArtifactDescriptorRequest
                                  ArtifactDescriptorResult
                                  ArtifactDescriptorException)
   (org.eclipse.aether.repository LocalRepository)
   (org.apache.maven.repository.internal MavenRepositorySystemUtils)
   (org.apache.maven.wagon.providers.http HttpWagon)
   (org.eclipse.aether.graph Dependency Exclusion DependencyNode)
   org.eclipse.aether.artifact.Artifact
   org.eclipse.aether.artifact.DefaultArtifact
   org.eclipse.aether.metadata.Metadata
   org.eclipse.aether.metadata.DefaultMetadata
   org.eclipse.aether.resolution.VersionResult
   org.eclipse.aether.resolution.VersionRequest
   org.eclipse.aether.resolution.VersionResolutionException
   org.eclipse.aether.repository.RemoteRepository
   (org.eclipse.aether.spi.connector RepositoryConnectorFactory)
   (org.eclipse.aether.connector.basic BasicRepositoryConnectorFactory)
   org.eclipse.aether.spi.connector.layout.RepositoryLayoutProvider
   org.eclipse.aether.spi.connector.layout.RepositoryLayout
   org.eclipse.aether.spi.connector.transport.GetTask
   org.eclipse.aether.spi.connector.transport.Transporter
   org.eclipse.aether.spi.connector.transport.TransporterProvider
   org.eclipse.aether.transfer.NoRepositoryLayoutException
   org.eclipse.aether.transfer.NoTransporterException
   (org.eclipse.aether.spi.connector.transport TransporterFactory)
   (org.eclipse.aether.transport.file FileTransporterFactory)
   (org.eclipse.aether.transport.wagon WagonProvider WagonTransporterFactory)
   org.eclipse.aether.util.version.GenericVersionScheme)
  (:require [clojure.java.io :as io]
            (leiningen.nix.deps
             [cons :as cons]
             [uncons :as uncons])))

(def default-repositories
  {"central" "http://repo1.maven.org/maven2/"
   "clojars" "https://clojars.org/repo/"})

(defn temp-local-repo []
  (-> (Files/createTempDirectory "m2-" (into-array FileAttribute []))
      .toFile (doto .deleteOnExit)
      cons/local-repository))

(defn home-local-repo []
  (-> "user.home" System/getProperty
      (io/file ".m2" "repository")
      cons/local-repository))

(defn- dependency-graph
  ([node]
   (reduce (fn [g ^DependencyNode n]
             (if-let [dep (.getDependency n)]
               (update-in g [(uncons/dep-spec dep)]
                          clojure.set/union
                          (->> (.getChildren n)
                               (map #(.getDependency %))
                               (map uncons/dep-spec)
                               set))
               g))
           {}
           (tree-seq (constantly true)
                     #(seq (.getChildren %))
                     node))))

(defn collect-deps [deps {:as config :keys [system session repositories]}]
  (-> (.collectDependencies system session (cons/collection-request deps config))
      .getRoot .getChildren))

(defn- art-key [art]
  [(.getGroupId art)
   (.getArtifactId art)
   (.getVersion art)
   (.getClassifier art)])

(defn download-info [arti {:as config}]
  (let [art (cons/artifact arti)
        desc (cons/artifact-descriptor art config)]
    
    (if-let [layout (try (cons/repository-layout (.getRepository desc) config)
                         (catch Exception e
                           (println "ERROR" "no download info" (art-key art) (.getRepository desc))))]
      (let [base (.. desc getRepository getUrl)
            loc (.getLocation layout art false)
            cs-loc (some #(when (= "SHA-1" (.getAlgorithm %))
                            (.getLocation %))
                         (.getChecksums layout art false loc))]
        {:art art
         :desc desc
         :location loc
         :cs-location cs-loc
         :base base
         :sha-1 (try (slurp (str base cs-loc))
                     (catch Exception e
                       (println "ERROR" "couldn't fetch sha-1" base cs-loc)
                       nil))})
      {:desc desc :art art})))

(defn get-dependencies [desc done {:keys [include-optionals include-scopes]}]
  (->> (cond-> (.getDependencies desc)
         (not include-optionals) (->> (remove #(.isOptional %))))
       (filter #(contains? include-scopes (.getScope %)))
       (remove #(contains? done (art-key (.getArtifact %))))))

(defn expand-download-info
  ([art conf] (expand-download-info art conf #{}))
  ([art {:keys [m-dl-info] :as conf} done]
   (lazy-seq
    (let [{:keys [desc] :as res} (m-dl-info art)
          sub-deps (get-dependencies desc done conf)
          done' (into done (cons (art-key (.getArtifact desc))
                                 (map #(art-key (.getArtifact %)) sub-deps)))
          
          thunks (mapv #(future
                          (let [di (expand-download-info
                                    (.getArtifact %) conf
                                    (disj done' (art-key (.getArtifact %))))]
                            (lazy-seq (cons (assoc (first di) :scope (.getScope %))
                                            (next di)))))
                       sub-deps)]
                                        ;[res (mapcat deref thunks)]
      (cons res (mapcat deref thunks))))))

(defn coordinate* [art]
  [(.getGroupId art)
   (.getArtifactId art)
   (.getVersion art)])

(defn checksums-for [download-infos]
  (reduce (fn [res {:keys [art sha-1]}]
            (update-in res (coordinate* art)
                       #(let [v ["mvn" sha-1]]
                          (when (and (some? %) (not= % v))
                            (throw (ex-info (str "SHA-1 mismatch " % v) {:old % :new v})))
                          v)))
          {} download-infos))

(defn dep-graph-for [download-infos config]
  (reduce (fn [res {:keys [desc art]}]
            (let [coo (coordinate* art)]
              (if (get-in res coo)
                res
                (let [deps (get-dependencies desc #{} config)]
                  (if (seq deps)
                    (assoc-in res coo
                              (mapv #(coordinate* (.getArtifact %))
                                    deps))
                    res)))))
          {} download-infos))

(defn repo-info [artifacts config]
  (let [dl (mapcat #(expand-download-info % config) artifacts)]
    {:checksums (checksums-for dl)
     :dependencies (dep-graph-for dl config)}))

(defn merge-aether-config [prev-config & {:as next-config}]
  (let [{:keys [system session local-repo offline transfer-listener
                mirror-selector repositories include-optionals include-scopes]
         :as config}
        (merge prev-config next-config)]
    (-> {:system (or system @cons/default-repo-system)
         :local-repo (or local-repo (temp-local-repo))
         :offline (boolean offline)
         :include-optionals (if (contains? config :include-optionals)
                              (boolean include-optionals)
                              true)
         :include-scopes (or include-scopes #{"compile" "runtime" "provided" "system" "test"})}
        (as-> config #__
          (assoc config :session (or session (cons/session config)))
          (assoc config :repositories (cons/repositories (or repositories default-repositories) config))
          
          (assoc config :m-dl-info
                 (memoize #(download-info % config)))))))

(defn aether-config [& {:keys [system session local-repo offline transfer-listener mirror-selector repositories] :as config}]
  (merge-aether-config config))


(comment

  (clojure.pprint/pprint
   (let [cfg (aether-config
              :include-optionals false
              :include-scopes #{"compile" "provided" "system"})]
     (repo-info
      '[[webnf.deps/logback "0.2.0-alpha1"]]
      cfg)))

  )

(def gvs (GenericVersionScheme.))

(defn- compare-versions [v1 v2]
  (compare (.parseVersion gvs v1)
           (.parseVersion gvs v2)))

(defn- unify-versions [result artifacts fixed-versions dependencies]
  (reduce (fn [r [group artifact version :as desc]]
            (let [version* (get-in r [group artifact])]
              (if (or (nil? version*) (neg? (compare-versions version version*)))
                (unify-versions (assoc-in r [group artifact] version)
                                (get-in dependencies [group artifact version])
                                fixed-versions dependencies)
                r)))
          result artifacts))

(defn- expand-deps* [artifacts version-map dependencies]
  (mapcat (fn [[group art _]]
            (let [version (get-in version-map [group art])]
              (cons [group art]
                    (expand-deps* (get-in dependencies [group art version])
                                  version-map dependencies))))
          artifacts))

(defn expand-deps [artifacts fixed-versions info]
  (let [version-map (unify-versions {} artifacts fixed-versions (:dependencies info))]
    (->> (expand-deps* artifacts version-map (:dependencies info))
         reverse distinct reverse
         (mapv (fn [[g a :as ga]]
                 [g a (get-in version-map ga)])))))

(comment

  (let [cfg (aether-config
             :include-optionals false
             :include-scopes #{"compile" #_"provided" #_"system"})
        info (repo-info
              '[[webnf.deps/logback "0.2.0-alpha1"]]
              cfg)]
    (clojure.pprint/pprint
     #_info
     (expand-deps [#_["webnf.deps" "logback" "0.2.0-alpha1"]
                   ["ch.qos.logback" "logback-classic" "1.1.8"]]
                  {} #_{"ch.qos.logback" {"logback-classic" "CUSTOM"}}
                  info)))

  )

(defn clj-dep->nix-dep [[group-artifact version]]
  [(or (namespace group-artifact) (name group-artifact))
   (name group-artifact)
   version])

(comment

  (require 'nix.data)

  (let [aether-deps '[[org.clojure/clojure "1.9.0-alpha14"]
                      [org.apache.maven/maven-aether-provider "3.3.9"]
                      [org.eclipse.aether/aether-transport-file "1.1.0"]
                      [org.eclipse.aether/aether-transport-wagon "1.1.0"]
                      [org.eclipse.aether/aether-connector-basic "1.1.0"]
                      [org.apache.maven.wagon/wagon-provider-api "2.10"]
                      [org.apache.maven.wagon/wagon-http "2.10"]
                      [org.apache.maven.wagon/wagon-ssh "2.10"]]
        expansion-deps '[[org.clojure/clojure "1.9.0-alpha14"]
                         [org.eclipse.aether/aether-util "1.1.0"]]

        cfg (aether-config
             :include-optionals false
             :include-scopes #{"compile"})
        repo (repo-info (concat aether-deps expansion-deps) cfg)]
    (spit "repo.nix" (nix.data/render repo))
    (spit "expansion.expanded.deps.nix" (nix.data/render
                                         (expand-deps (map clj-dep->nix-dep expansion-deps)
                                                      {} repo)))
    (spit "aether.deps.nix" (nix.data/render (mapv clj-dep->nix-dep aether-deps))))

  )
