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
   org.apache.maven.artifact.versioning.DefaultArtifactVersion
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
            [clojure.string :as str]
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

(defn memoize-singular [f]
  (let [memo (atom {})]
    (fn [& args]
      (if-let [v (get @memo args)]
        @v
        @(get (swap! memo (fn [m]
                            (let [prev-delay (get m args)]
                              (if (nil? prev-delay)
                                (assoc m args (delay (apply f args)))
                                m))))
              args)))))

(defn collect-deps [deps {:as config :keys [system session repositories]}]
  (-> (.collectDependencies system session (cons/collection-request deps config))
      .getRoot .getChildren))

(defn- art-key [art]
  [(.getGroupId art)
   (.getArtifactId art)
   (.getVersion art)
   (.getClassifier art)])

(defn coordinate* [art]
  [(.getGroupId art)
   (.getArtifactId art)
   (.getVersion art)])

(defn get-dependencies [desc done {:keys [include-optionals include-scopes]}]
  (->> (cond-> (.getDependencies desc)
         (not include-optionals) (->> (remove #(.isOptional %))))
       (filter #(contains? include-scopes (.getScope %)))
       (remove #(contains? done (art-key (.getArtifact %))))))

(defn download-info [arti {:as config}]
  (let [art (cons/artifact arti)
        rv (.getVersion (or (cons/version-resolution art config)
                            art))
        rart (cons/artifact [(keyword (.getGroupId art)
                                      (.getArtifactId art))
                             rv])
        desc (cons/artifact-descriptor rart config)]
    
    (if-let [layout (try (cons/repository-layout (.getRepository desc) config)
                         (catch Exception e
                           (println "ERROR" "no download info" (art-key rart) (.getRepository desc))))]
      (let [base (.. desc getRepository getUrl)
            loc (.getLocation layout rart false)
            cs-loc (some #(when (= "SHA-1" (.getAlgorithm %))
                            (.getLocation %))
                         (.getChecksums layout rart false loc))]
        (cond-> {:art art
                 :desc desc
                 :location loc
                 :cs-location cs-loc
                 :base base
                 :coord (coordinate* art)
                 :deps (mapv #(coordinate* (.getArtifact %))
                             (get-dependencies desc #{} config))
                 :sha1 (try (slurp (str base cs-loc))
                            (catch Exception e
                              (println "ERROR" "couldn't fetch sha-1" base cs-loc)
                              nil))}
          (.isSnapshot art) (assoc :snapshot true)
          (not= rv (.getVersion art)) (assoc :resolved-version rv)))
      {:desc desc :art art})))

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

(defn expand-download-infos
  ([artifacts conf] (mapcat #(expand-download-info % conf) artifacts)))

(defn checksums-for [download-infos]
  (reduce (fn [res {:keys [art sha1]}]
            (update-in res (coordinate* art)
                       #(let [v ["mvn" sha1]]
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

(defn index-download-infos [download-infos]
  (reduce (fn [res {:as dli :keys [art]}]
            (assoc-in res (coordinate* art) dli))
          {} download-infos))

(defn map-vals [f m]
  (into {} (map (juxt key (comp f val)) m)))

(defn max-key*
  "Returns the x for which (k x), is greatest."
  {:added "1.0"
   :static true}
  ([k x] x)
  ([k x y] (if (pos? (compare (k x) (k y))) x y))
  ([k x y & more]
   (reduce #(max-key* k %1 %2) (max-key* k x y) more)))

(defn max-version [version-map]
  (apply max-key* cons/version (keys version-map)))

(defn add-defaults [g-map]
  (map-vals
   (fn [a-map]
     (map-vals
      (fn [v-map]
        (assoc v-map "DEFAULT"
               ["alias" (max-version v-map)]))
      a-map))
   g-map))

(defn repo-info [artifacts config]
  (let [dl (expand-download-infos artifacts config)]
    {:checksums (add-defaults (checksums-for dl))
     :roots (mapv (comp (juxt #(.getGroupId %)
                              #(.getArtifactId %)
                              #(.getVersion %))
                        cons/artifact)
                  artifacts)
     :dependencies (dep-graph-for dl config)}))

(defn classpath-for* [dl-infos config]
  (fn this [init roots]
    (reduce (fn [init' [group artifact orig-version]]
              (if (get-in init' [:added group artifact])
                (update-in init' [:added group artifact]
                           (fn [{:keys [updated-from]
                                 [_ _ version] :coord
                                 :or {updated-from #{}}
                                 :as dli}]
                             (cond-> dli (not= version orig-version)
                                     (assoc :updated-from (conj updated-from orig-version)))))
                (let [version (max-version (get-in dl-infos [group artifact]))
                      dli (get-in dl-infos [group artifact version])]
                  (-> init'
                      (assoc-in [:added group artifact] dli)
                      (this (:deps dli))
                      (update :suffix conj [group artifact])))))
            init (rseq roots))))

(defn classpath-for [roots config]
  (let [dl (index-download-infos
            (expand-download-infos roots config))
        {:keys [suffix added]} ((classpath-for* dl config)
                                {:suffix () :added {}}
                                (mapv #(coordinate* (cons/artifact %)) roots))]
    (for [s suffix
          :let [{:keys [art loc updated-from snapshot] :as dli}
                (get-in added s)]]
      (select-keys dli [:coord :sha1 :updated-from :resolved-version]))))

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
                 (memoize-singular #(download-info % config)))))))

(defn aether-config [& {:keys [system session local-repo offline transfer-listener mirror-selector repositories] :as config}]
  (merge-aether-config config))


(comment
  (def cfg (aether-config
            :include-optionals false
            :include-scopes #{"compile" #_"provided"}))
  (def cp (classpath-for '[[org.clojure/clojure "1.9.0-alpha14"]
                           [webnf/base "0.2.0-SNAPSHOT"]
                           [org.apache.maven/maven-aether-provider "3.3.9"]
                           [org.eclipse.aether/aether-transport-file "1.1.0"]
                           [org.eclipse.aether/aether-transport-wagon "1.1.0"]
                           [org.eclipse.aether/aether-connector-basic "1.1.0"]
                           [org.apache.maven.wagon/wagon-provider-api "2.10"]
                           [org.apache.maven.wagon/wagon-http "2.10"]
                           [org.apache.maven.wagon/wagon-ssh "2.10"]]
                         cfg))
  )


#_(comment
    (def ri (let [cfg (aether-config
                       :include-optionals false
                       :include-scopes #{"compile" #_"provided"})]
              (repo-info
               '[#_[org.eclipse.jetty/jetty-webapp "9.3.6.v20151106"]
                 #_[webnf.deps/logback "0.1.18"]
                 #_[webnf/base "0.1.18"]
                 [org.clojure/clojure "1.9.0-alpha14"]
                 [org.apache.maven/maven-aether-provider "3.3.9"]
                 [org.eclipse.aether/aether-transport-file "1.1.0"]
                 [org.eclipse.aether/aether-transport-wagon "1.1.0"]
                 [org.eclipse.aether/aether-connector-basic "1.1.0"]
                 [org.apache.maven.wagon/wagon-provider-api "2.10"]
                 [org.apache.maven.wagon/wagon-http "2.10"]
                 [org.apache.maven.wagon/wagon-ssh "2.10"]]
               cfg)))
    (clojure.pprint/pprint ri)
    (require 'nix.data)
    (print (nix.data/render ri)))

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
