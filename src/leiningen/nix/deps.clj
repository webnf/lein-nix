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
   (org.eclipse.aether.transport.wagon WagonProvider WagonTransporterFactory))
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
      '[[webnf.deps/logback "0.1.18"]]
      cfg))))

#_(take 2 (expand-download-info '[webnf.deps/universe "0.1.18"]
                                (aether-config)))

#_(comment

    (def dep-node (collect-deps
                   '[[webnf.deps/universe "0.2.0-SNAPSHOT"]]
                   (aether-config)))

    (-> dep-node first
        .getDependency
        .getArtifact
        (ArtifactDescriptorRequest. ))

    (def art-desc
      (cons/artifact-descriptor '[webnf/base "0.1.18"] ;;'[org.springframework/spring-beans "1.0-m4"]
                                (aether-config)))

    (.getDependencies art-desc)
    (.getRepository art-desc)
  

    (cons/repository-layout (cons/repository (first default-repositories)) (aether-config))
  
    (download-info '[webnf.deps/universe "0.1.18"]
                   (aether-config))

    (aether-config)
    )
