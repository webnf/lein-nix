(ns leiningen.nix.aether-impl
  (:refer-clojure :exclude  [type proxy])
  (:require [clojure.java.io :as io]
            clojure.set
            [clojure.string :as str]
            clojure.stacktrace)
  (:import (org.apache.maven.repository.internal MavenRepositorySystemUtils)
           (org.apache.maven.wagon.providers.http HttpWagon)
           (org.eclipse.aether RepositorySystem)
           (org.eclipse.aether.transfer TransferListener)
           (org.eclipse.aether.artifact Artifact)
           
           (org.eclipse.aether.spi.connector.transport TransporterFactory)
           (org.eclipse.aether.transport.file FileTransporterFactory)
           (org.eclipse.aether.transport.wagon WagonProvider WagonTransporterFactory)

           (org.eclipse.aether.spi.connector RepositoryConnectorFactory)
           (org.eclipse.aether.connector.basic BasicRepositoryConnectorFactory)
           
           (org.eclipse.aether.repository Proxy ArtifactRepository Authentication
                                          RepositoryPolicy LocalRepository RemoteRepository
                                          MirrorSelector RemoteRepository$Builder)
           (org.eclipse.aether.util.repository DefaultProxySelector DefaultMirrorSelector)
           (org.eclipse.aether.graph Dependency Exclusion DependencyNode)
           (org.eclipse.aether.collection CollectRequest)
           (org.eclipse.aether.resolution DependencyRequest ArtifactRequest
                                          ArtifactResult VersionRequest)
           (org.eclipse.aether.artifact DefaultArtifact ArtifactProperties)
           (org.eclipse.aether.util.graph.visitor PreorderNodeListGenerator)
           (org.eclipse.aether.util.artifact SubArtifact)
           (org.eclipse.aether.deployment DeployRequest)
           (org.eclipse.aether.installation InstallRequest)
           (org.eclipse.aether.util.version GenericVersionScheme)))

(def ^{:private true} default-local-repo
  (io/file (System/getProperty "user.home") ".m2" "repository"))

(def maven-central {"central" "http://repo1.maven.org/maven2/"})

; Using HttpWagon (which uses apache httpclient) because the "LightweightHttpWagon"
; (which just uses JDK HTTP) reliably flakes if you attempt to resolve SNAPSHOT
; artifacts from an HTTPS password-protected repository (like a nexus instance)
; when other un-authenticated repositories are included in the resolution.
; My theory is that the JDK HTTP impl is screwing up connection pooling or something,
; and reusing the same connection handle for the HTTPS repo as it used for e.g.
; central, without updating the authentication info.
; In any case, HttpWagon is what Maven 3 uses, and it works.
(def ^{:private true} wagon-factories (atom {"http" #(HttpWagon.)
                                             "https" #(HttpWagon.)}))

(defn register-wagon-factory!
  "Registers a new no-arg factory function for the given scheme.  The function must return
   an implementation of org.apache.maven.wagon.Wagon."
  [scheme factory-fn]
  (swap! wagon-factories (fn [m]
                           (when-let [fn (m scheme)]
                             (println (format "Warning: replacing existing support for %s repositories (%s) with %s" scheme fn factory-fn)))
                           (assoc m scheme factory-fn))))

(deftype PomegranateWagonProvider []
  WagonProvider
  (release [_ wagon])
  (lookup [_ role-hint]
    (when-let [f (get @wagon-factories role-hint)]
      (try 
        (f)
        (catch Exception e
          (clojure.stacktrace/print-cause-trace e)
          (throw e))))))

(deftype TransferListenerProxy [listener-fn]
  TransferListener
  (transferCorrupted [_ e] (listener-fn e))
  (transferFailed [_ e] (listener-fn e))
  (transferInitiated [_ e] (listener-fn e))
  (transferProgressed [_ e] (listener-fn e))
  (transferStarted [_ e] (listener-fn e))
  (transferSucceeded [_ e] (listener-fn e)))

(defn- transfer-event
  [^org.eclipse.aether.transfer.TransferEvent e]
  ; INITIATED, STARTED, PROGRESSED, CORRUPTED, SUCCEEDED, FAILED
  {:type (-> e .getType .name str/lower-case keyword)
   ; :get :put
   :method (-> e .getRequestType str/lower-case keyword)
   :transferred (.getTransferredBytes e)
   :error (.getException e)
   :data-buffer (.getDataBuffer e)
   :data-length (.getDataLength e)
   :resource (let [r (.getResource e)]
               {:repository (.getRepositoryUrl r)
                :name (.getResourceName r)
                :file (.getFile r)
                :size (.getContentLength r)
                :transfer-start-time (.getTransferStartTime r)
                :trace (.getTrace r)})})

(defn- default-listener-fn
  [{:keys [type method transferred resource error] :as evt}]
  (let [{:keys [name size repository transfer-start-time]} resource]
    (case type
      :started (do
                 (print (case method :get "Retrieving" :put "Sending")
                        name
                        (if (neg? size)
                          ""
                          (format "(%sk)" (Math/round (double (max 1 (/ size 1024)))))))
                 (when (< 70 (+ 10 (count name) (count repository)))
                   (println) (print "    "))
                 (println (case method :get "from" :put "to") repository))
      (:corrupted :failed) (when error (println (.getMessage error)))
      nil)))

(defn- repository-system
  []
  (.getService (doto (MavenRepositorySystemUtils/newServiceLocator)
                 (.addService RepositoryConnectorFactory BasicRepositoryConnectorFactory)
                 (.addService TransporterFactory FileTransporterFactory)
                 (.addService TransporterFactory WagonTransporterFactory)
                 (.addService WagonProvider PomegranateWagonProvider))
               org.eclipse.aether.RepositorySystem))

(defn- construct-transfer-listener
  [transfer-listener]
  (cond
    (instance? TransferListener transfer-listener) transfer-listener

    (= transfer-listener :stdout)
    (TransferListenerProxy. (comp default-listener-fn transfer-event))

    (fn? transfer-listener)
    (TransferListenerProxy. (comp transfer-listener transfer-event))

    :else (TransferListenerProxy. (fn [_]))))

(defn repository-session
  [{:keys [repository-system local-repo offline? transfer-listener mirror-selector]}]
  (let [session (MavenRepositorySystemUtils/newSession)]
    (-> session
        (.setLocalRepositoryManager (.newLocalRepositoryManager repository-system session
                                                                (-> (io/file (or local-repo default-local-repo))
                                                                    .getAbsolutePath
                                                                    LocalRepository.)))
        (.setMirrorSelector mirror-selector)
        (.setOffline (boolean offline?))
        (.setTransferListener (construct-transfer-listener transfer-listener)))))

(def update-policies {:daily RepositoryPolicy/UPDATE_POLICY_DAILY
                      :always RepositoryPolicy/UPDATE_POLICY_ALWAYS
                      :never RepositoryPolicy/UPDATE_POLICY_NEVER})

(def checksum-policies {:fail RepositoryPolicy/CHECKSUM_POLICY_FAIL
                        :ignore RepositoryPolicy/CHECKSUM_POLICY_IGNORE
                        :warn RepositoryPolicy/CHECKSUM_POLICY_WARN})

(defn- policy
  [policy-settings enabled?]
  (RepositoryPolicy.
    (boolean enabled?)
    (update-policies (:update policy-settings :daily))
    (checksum-policies (:checksum policy-settings :fail))))

#_(defn- set-policies
    [repo settings]
    (doto repo
      (.setPolicy true (policy settings (:snapshots settings true)))
      (.setPolicy false (policy settings (:releases settings true)))))

#_(defn- set-authentication
    "Calls the setAuthentication method on obj"
    [obj {:keys [username password passphrase private-key-file] :as settings}]
    (if (or username password private-key-file passphrase)
      ;; FIXME see org.eclipse.aether.repository.AuthenticationContext
      (.setAuthentication obj (Authentication. username password private-key-file passphrase))
      obj))

;; FIXME set-authentication
#_(defn- set-proxy 
    [repo {:keys [type host port non-proxy-hosts ] 
           :or {type "http"} 
           :as proxy} ]
    (if (and repo host port)
      (let [prx-sel (doto (DefaultProxySelector.)
                      (.add (set-authentication (Proxy. type host port nil) proxy)
                            non-proxy-hosts))
            prx (.getProxy prx-sel repo)]
        (.setProxy repo prx))
      repo))

(defn- make-repository
  [[id settings] proxy]
  (let [settings-map (if (string? settings)
                       {:url settings}
                       settings)]
    #_(doto (RemoteRepository. id
                               (:type settings-map "default")
                               (str (:url settings-map)))
        (set-policies settings-map)
        ;; FIXME set-authentication
                                        ;(set-proxy proxy)
                                        ;(set-authentication settings-map)
        )
    (.. (RemoteRepository$Builder. id
                                   (:type settings-map "default")
                                   (str (:url settings-map)))
        (setPolicy (policy settings (:releases settings true)))
        (setSnapshotPolicy (policy settings (:snapshots settings true)))
        build)))

(defn- group
  [group-artifact]
  (or (namespace group-artifact) (name group-artifact)))


(defn- coordinate-string
  "Produces a coordinate string with a format of
   <groupId>:<artifactId>[:<extension>[:<classifier>]]:<version>>
   given a lein-style dependency spec.  :extension defaults to jar."
  [[group-artifact version & {:keys [classifier extension] :or {extension "jar"}}]]
  (->> [(group group-artifact) (name group-artifact) extension classifier version]
    (remove nil?)
    (interpose \:)
    (apply str)))

(defn- exclusion
  [[group-artifact & {:as opts}]]
  (Exclusion.
    (group group-artifact)
    (name group-artifact)
    (:classifier opts "*")
    (:extension opts "*")))

(defn- normalize-exclusion-spec [spec]
  (if (symbol? spec)
    [spec]
    spec))

(defn- artifact
  [[group-artifact version & {:keys [scope optional exclusions]} :as dep-spec]]
  (DefaultArtifact. (coordinate-string dep-spec)))

(defn- dependency
  [[group-artifact version & {:keys [scope optional exclusions]
                              :as opts
                              :or {scope "compile"
                                   optional false}}
    :as dep-spec]]
  (Dependency. (artifact dep-spec)
               scope
               optional
               (map (comp exclusion normalize-exclusion-spec) exclusions)))

(declare dep-spec*)

(defn- exclusion-spec
  "Given an Aether Exclusion, returns a lein-style exclusion vector with the
   :exclusion in its metadata."
  [^Exclusion ex]
  (with-meta (-> ex bean dep-spec*) {:exclusion ex}))

(defn- dep-spec
  "Given an Aether Dependency, returns a lein-style dependency vector with the
   :dependency and its corresponding artifact's :file in its metadata."
  [^Dependency dep]
  (let [artifact (.getArtifact dep)]
    (-> (merge (bean dep) (bean artifact))
      dep-spec*
      (with-meta {:dependency dep :file (.getFile artifact)}))))

(defn- dep-spec*
  "Base function for producing lein-style dependency spec vectors for dependencies
   and exclusions."
  [{:keys [groupId artifactId version classifier extension scope optional exclusions]
    :or {version nil
         scope "compile"
         optional false
         exclusions nil}}]
  (let [group-artifact (apply symbol (if (= groupId artifactId)
                                       [artifactId]
                                       [groupId artifactId]))]
    (vec (concat [group-artifact]
                 (when version [version])
                 (when (and (seq classifier)
                            (not= "*" classifier))
                   [:classifier classifier])
                 (when (and (seq extension)
                            (not (#{"*" "jar"} extension)))
                   [:extension extension])
                 (when optional [:optional true])
                 (when (not= scope "compile")
                   [:scope scope])
                 (when (seq exclusions)
                   [:exclusions (vec (map exclusion-spec exclusions))])))))

(defn- create-artifact
  [files artifact]
  (if-let [file (get files artifact)]
    (-> (coordinate-string artifact)
      DefaultArtifact.
      (.setFile (io/file file)))
    (throw (IllegalArgumentException. (str "No file provided for artifact " artifact)))))

(defn deploy-artifacts
  "Deploy the artifacts kwarg to the repository kwarg.

  :files - map from artifact vectors to file paths or java.io.File objects
           where the file to be deployed for each artifact is to be found
           An artifact vector is e.g.
             '[group/artifact \"1.0.0\"] or
             '[group/artifact \"1.0.0\" :extension \"pom\"].
           All artifacts should have the same version and group and artifact IDs
  :repository - {name url} | {name settings}
    settings:
      :url - URL of the repository
      :snapshots - use snapshots versions? (default true)
      :releases - use release versions? (default true)
      :username - username to log in with
      :password - password to log in with
      :passphrase - passphrase to log in wth
      :private-key-file - private key file to log in with
      :update - :daily (default) | :always | :never
      :checksum - :fail | :ignore | :warn (default)
  :local-repo - path to the local repository (defaults to ~/.m2/repository)
  :transfer-listener - same as provided to resolve-dependencies

  :proxy - proxy configuration, can be nil, the host scheme and type must match
    :host - proxy hostname
    :type - http  (default) | http | https
    :port - proxy port
    :non-proxy-hosts - The list of hosts to exclude from proxying, may be null
    :username - username to log in with, may be null
    :password - password to log in with, may be null
    :passphrase - passphrase to log in wth, may be null
    :private-key-file - private key file to log in with, may be null"

  [& {:keys [files repository local-repo transfer-listener proxy repository-session-fn]}]
  (when (empty? files)
    (throw (IllegalArgumentException. "Must provide valid :files to deploy-artifacts")))
  (when (->> (keys files)
             (map (fn [[ga v]] [(if (namespace ga) ga (symbol (str ga) (str ga))) v]))
             set
             count
             (< 1))
    (throw (IllegalArgumentException.
            (str "Provided artifacts have varying version, group, or artifact IDs: " (keys files)))))
  (let [system (repository-system)
        session ((or repository-session-fn
                     repository-session)
                 {:repository-system system
                  :local-repo local-repo
                  :offline? false
                  :transfer-listener transfer-listener})]
    (.deploy system session
             (doto (DeployRequest.)
               (.setArtifacts (vec (map (partial create-artifact files) (keys files))))
               (.setRepository (first (map #(make-repository % proxy) repository)))))))

(defn install-artifacts
  "Deploy the file kwarg using the coordinates kwarg to the repository kwarg.

  :files - same as with deploy-artifacts
  :local-repo - path to the local repository (defaults to ~/.m2/repository)
  :transfer-listener - same as provided to resolve-dependencies"
  [& {:keys [files local-repo transfer-listener repository-session-fn]}]
  (let [system (repository-system)
        session ((or repository-session-fn
                     repository-session)
                 {:repository-system system
                  :local-repo local-repo
                  :offline? false
                  :transfer-listener transfer-listener})]
    (.install system session
              (doto (InstallRequest.)
                (.setArtifacts (vec (map (partial create-artifact files) (keys files))))))))

(defn- artifacts-for
  "Takes a coordinates map, an a map from partial coordinates to "
  [coordinates file-map]
  (zipmap (map (partial into coordinates) (keys file-map)) (vals file-map)))

(defn- optional-artifact
  "Takes a coordinates map, an a map from partial coordinates to "
  [artifact-coords path]
  (when path {artifact-coords path}))

(defn deploy
  "Deploy the jar-file kwarg using the pom-file kwarg and coordinates
kwarg to the repository kwarg.

  :coordinates - [group/name \"version\"]
  :artifact-map - a map from partial coordinates to file path or File
  :jar-file - a file pointing to the jar
  :pom-file - a file pointing to the pom
  :repository - {name url} | {name settings}
    settings:
      :url - URL of the repository
      :snapshots - use snapshots versions? (default true)
      :releases - use release versions? (default true)
      :username - username to log in with
      :password - password to log in with
      :passphrase - passphrase to log in wth
      :private-key-file - private key file to log in with
      :update - :daily (default) | :always | :never
      :checksum - :fail (default) | :ignore | :warn

  :local-repo - path to the local repository (defaults to ~/.m2/repository)
  :transfer-listener - same as provided to resolve-dependencies

  :proxy - proxy configuration, can be nil, the host scheme and type must match
    :host - proxy hostname
    :type - http  (default) | http | https
    :port - proxy port
    :non-proxy-hosts - The list of hosts to exclude from proxying, may be null
    :username - username to log in with, may be null
    :password - password to log in with, may be null
    :passphrase - passphrase to log in wth, may be null
    :private-key-file - private key file to log in with, may be null"
  [& {:keys [coordinates artifact-map jar-file pom-file] :as opts}]
  (when (empty? coordinates)
    (throw
     (IllegalArgumentException. "Must provide valid :coordinates to deploy")))
  (apply deploy-artifacts
    (apply concat (assoc opts
                    :files (artifacts-for
                            coordinates
                            (merge
                             artifact-map
                             (optional-artifact [:extension "pom"] pom-file)
                             (optional-artifact [] jar-file)))))))

(defn install
  "Install the artifacts specified by the jar-file or file-map and pom-file
   kwargs using the coordinates kwarg.

  :coordinates - [group/name \"version\"]
  :artifact-map - a map from partial coordinates to file path or File
  :jar-file - a file pointing to the jar
  :pom-file - a file pointing to the pom
  :local-repo - path to the local repository (defaults to ~/.m2/repository)
  :transfer-listener - same as provided to resolve-dependencies"
  [& {:keys [coordinates artifact-map jar-file pom-file] :as opts}]
  (when (empty? coordinates)
    (throw
     (IllegalArgumentException. "Must provide valid :coordinates to install")))
  (apply install-artifacts
    (apply concat (assoc opts
                    :files (artifacts-for
                            coordinates
                            (merge
                             artifact-map
                             (optional-artifact [:extension "pom"] pom-file)
                             (optional-artifact [] jar-file)))))))

(defn- dependency-graph
  ([node]
    (reduce (fn [g ^DependencyNode n]
              (if-let [dep (.getDependency n)]
                (update-in g [(dep-spec dep)]
                           clojure.set/union
                           (->> (.getChildren n)
                             (map #(.getDependency %))
                             (map dep-spec)
                             set))
                g))
            {}
            (tree-seq (constantly true)
                      #(seq (.getChildren %))
                      node))))

(defn- mirror-selector-fn
  "Default mirror selection function.  The first argument should be a map
   like that described as the :mirrors argument in resolve-dependencies.
   The second argument should be a repository spec, also as described in
   resolve-dependencies.  Will return the mirror spec that matches the
   provided repository spec."
  [mirrors {:keys [name url snapshots releases]}]
  (let [mirrors (filter (fn [[matcher mirror-spec]]
                          (or
                            (and (string? matcher) (or (= matcher name) (= matcher url)))
                            (and (instance? java.util.regex.Pattern matcher)
                                 (or (re-matches matcher name) (re-matches matcher url)))))
                        mirrors)]
    (case (count mirrors)
      0 nil
      1 (-> mirrors first second)
      (if (some nil? (map second mirrors))
        ;; wildcard override
        nil
        (throw (IllegalArgumentException.
               (str "Multiple mirrors configured to match repository " {name url} ": "
                 (into {} (map #(update-in % [1] select-keys [:name :url]) mirrors)))))))))

(defn- mirror-selector
  "Returns a MirrorSelector that delegates matching of mirrors to given remote repositories
   to the provided function.  Any returned repository specifications are turned into
   RemoteRepository instances, and configured to use the provided proxy."
  [mirror-selector-fn proxy]
  (reify MirrorSelector
    (getMirror [_ repo]
      (let [repo-spec {:name (.getId repo)
                       :url (.getUrl repo)
                       :snapshots (-> repo (.getPolicy true) .isEnabled)
                       :releases (-> repo (.getPolicy false) .isEnabled)}
            
            {:keys [name repo-manager content-type] :as mirror-spec}
            (mirror-selector-fn repo-spec)]
        (when-let [mirror (and mirror-spec (make-repository [name mirror-spec] proxy))]
        (-> (.setMirroredRepositories mirror [repo])
          (.setRepositoryManager (boolean repo-manager))
          (.setContentType (or content-type "default"))))))))

(defn resolve-artifacts*
  "Resolves artifacts for the coordinates kwarg, using repositories from the
   `:repositories` kwarg.

   Retrieval of dependencies can be disabled by providing `:retrieve false` as a
   kwarg.

   Returns an sequence of either `org.eclipse.aether.VersionResult`
   if `:retrieve false`, or `org.eclipse.aether.ArtifactResult` if
   `:retrieve true` (the default).

   If you don't want to mess with the Aether implementation classes, then use
   `resolve-artifacts` instead.

    :coordinates - [[group/name \"version\" & settings] ..]
      settings:
      :extension  - the maven extension (type) to require
      :classifier - the maven classifier to require
      :scope      - the maven scope for the dependency (default \"compile\")
      :optional   - is the dependency optional? (default \"false\")
      :exclusions - which sub-dependencies to skip : [group/name & settings]
        settings:
        :classifier (default \"*\")
        :extension  (default \"*\")

    :repositories - {name url ..} | {name settings ..}
      (defaults to {\"central\" \"http://repo1.maven.org/maven2/\"}
      settings:
      :url - URL of the repository
      :snapshots - use snapshots versions? (default true)
      :releases - use release versions? (default true)
      :username - username to log in with
      :password - password to log in with
      :passphrase - passphrase to log in wth
      :private-key-file - private key file to log in with
      :update - :daily (default) | :always | :never
      :checksum - :fail (default) | :ignore | :warn

    :local-repo - path to the local repository (defaults to ~/.m2/repository)
    :offline? - if true, no remote repositories will be contacted
    :transfer-listener - the transfer listener that will be notifed of dependency
      resolution and deployment events.
      Can be:
        - nil (the default), i.e. no notification of events
        - :stdout, corresponding to a default listener implementation that writes
            notifications and progress indicators to stdout, suitable for an
            interactive console program
        - a function of one argument, which will be called with a map derived from
            each event.
        - an instance of org.eclipse.aether.transfer.TransferListener

    :proxy - proxy configuration, can be nil, the host scheme and type must match
      :host - proxy hostname
      :type - http  (default) | http | https
      :port - proxy port
      :non-proxy-hosts - The list of hosts to exclude from proxying, may be null
      :username - username to log in with, may be null
      :password - password to log in with, may be null
      :passphrase - passphrase to log in wth, may be null
      :private-key-file - private key file to log in with, may be null

    :mirrors - {matches settings ..}
      matches - a string or regex that will be used to match the mirror to
                candidate repositories. Attempts will be made to match the
                string/regex to repository names and URLs, with exact string
                matches preferred. Wildcard mirrors can be specified with
                a match-all regex such as #\".+\".  Excluding a repository
                from mirroring can be done by mapping a string or regex matching
                the repository in question to nil.
      settings include these keys, and all those supported by :repositories:
      :name         - name/id of the mirror
      :repo-manager - whether the mirror is a repository manager"

  [& {:keys [repositories coordinates files retrieve local-repo
             transfer-listener offline? proxy mirrors repository-session-fn]
      :or {retrieve true}}]
  (let [repositories (or repositories maven-central)
        system (repository-system)
        mirror-selector-fn (memoize (partial mirror-selector-fn mirrors))
        mirror-selector (mirror-selector mirror-selector-fn proxy)
        session ((or repository-session-fn
                     repository-session)
                 {:repository-system system
                  :local-repo local-repo
                  :offline? offline?
                  :transfer-listener transfer-listener
                  :mirror-selector mirror-selector})
        deps (->> coordinates
                  (map #(if-let [local-file (get files %)]
                          (-> (artifact %)
                              (.setProperties
                               {ArtifactProperties/LOCAL_PATH
                                (.getPath (io/file local-file))}))
                          (artifact %)))
                  vec)
        repositories (vec (map #(let [repo (make-repository % proxy)]
                                  (-> session
                                      (.getMirrorSelector)
                                      (.getMirror repo)
                                      (or repo)))
                               repositories))]
    (if retrieve
      (.resolveArtifacts
       system session (map #(ArtifactRequest. % repositories nil) deps))
      (doall
       (for [dep deps]
         (.resolveVersion
          system session (VersionRequest. dep repositories nil)))))))

(defn resolve-artifacts
  "Same as `resolve-artifacts*`, but returns a sequence of dependencies; each
   artifact's metadata contains the source Aether result object, and the
   artifact's :file on disk."
  [& args]
  (let [{:keys [coordinates]} (apply hash-map args)]
    (->> (apply resolve-artifacts* args)
         (map
          (fn [coord result]
            {:pre [coord result]}
            (let [m (when (instance? ArtifactResult result)
                      {:file (.. ^ArtifactResult result getArtifact getFile)})]
              (with-meta coord
                (merge {:result result} m))))
          coordinates))))

(defn resolve-dependencies*
  "Collects dependencies for the coordinates kwarg, using repositories from the
   `:repositories` kwarg.
   Retrieval of dependencies can be disabled by providing `:retrieve false` as a kwarg.
   Returns an instance of either `org.eclipse.aether.collection.CollectResult` if
   `:retrieve false` or `org.eclipse.aether.resolution.DependencyResult` if
   `:retrieve true` (the default).  If you don't want to mess with the Aether
   implmeentation classes, then use `resolve-dependencies` instead.   

    :coordinates - [[group/name \"version\" & settings] ..]
      settings:
      :extension  - the maven extension (type) to require
      :classifier - the maven classifier to require
      :scope      - the maven scope for the dependency (default \"compile\")
      :optional   - is the dependency optional? (default \"false\")
      :exclusions - which sub-dependencies to skip : [group/name & settings]
        settings:
        :classifier (default \"*\")
        :extension  (default \"*\")

    :repositories - {name url ..} | {name settings ..}
      (defaults to {\"central\" \"http://repo1.maven.org/maven2/\"}
      settings:
      :url - URL of the repository
      :snapshots - use snapshots versions? (default true)
      :releases - use release versions? (default true)
      :username - username to log in with
      :password - password to log in with
      :passphrase - passphrase to log in wth
      :private-key-file - private key file to log in with
      :update - :daily (default) | :always | :never
      :checksum - :fail (default) | :ignore | :warn

    :local-repo - path to the local repository (defaults to ~/.m2/repository)
    :offline? - if true, no remote repositories will be contacted
    :transfer-listener - the transfer listener that will be notifed of dependency
      resolution and deployment events.
      Can be:
        - nil (the default), i.e. no notification of events
        - :stdout, corresponding to a default listener implementation that writes
            notifications and progress indicators to stdout, suitable for an
            interactive console program
        - a function of one argument, which will be called with a map derived from
            each event.
        - an instance of org.eclipse.aether.transfer.TransferListener

    :proxy - proxy configuration, can be nil, the host scheme and type must match 
      :host - proxy hostname
      :type - http  (default) | http | https
      :port - proxy port
      :non-proxy-hosts - The list of hosts to exclude from proxying, may be null
      :username - username to log in with, may be null
      :password - password to log in with, may be null
      :passphrase - passphrase to log in wth, may be null
      :private-key-file - private key file to log in with, may be null

    :mirrors - {matches settings ..}
      matches - a string or regex that will be used to match the mirror to
                candidate repositories. Attempts will be made to match the
                string/regex to repository names and URLs, with exact string
                matches preferred. Wildcard mirrors can be specified with
                a match-all regex such as #\".+\".  Excluding a repository
                from mirroring can be done by mapping a string or regex matching
                the repository in question to nil.
      settings include these keys, and all those supported by :repositories:
      :name         - name/id of the mirror
      :repo-manager - whether the mirror is a repository manager"

  [& {:keys [repositories coordinates files retrieve local-repo
             transfer-listener offline? proxy mirrors repository-session-fn]
      :or {retrieve true}}]
  (let [repositories (or repositories maven-central)
        system (repository-system)
        mirror-selector-fn (memoize (partial mirror-selector-fn mirrors))
        mirror-selector (mirror-selector mirror-selector-fn proxy)
        session ((or repository-session-fn
                     repository-session)
                 {:repository-system system
                  :local-repo local-repo
                  :offline? offline?
                  :transfer-listener transfer-listener
                  :mirror-selector mirror-selector})
        deps (->> coordinates
               (map #(if-let [local-file (get files %)]
                       (.setArtifact (dependency %)
                         (-> (dependency %)
                           .getArtifact
                           (.setProperties {ArtifactProperties/LOCAL_PATH
                                            (.getPath (io/file local-file))})))
                       (dependency %)))
               vec)
        collect-request (doto (CollectRequest. deps
                                nil
                                (vec (map #(let [repo (make-repository % proxy)]
                                             (-> session
                                               (.getMirrorSelector)
                                               (.getMirror repo)
                                               (or repo)))
                                       repositories)))
                          (.setRequestContext "runtime"))]
    (if retrieve
      (.resolveDependencies system session (DependencyRequest. collect-request nil))
      (.collectDependencies system session collect-request))))

(defn resolve-dependencies
  "Same as `resolve-dependencies*`, but returns a graph of dependencies; each
   dependency's metadata contains the source Aether Dependency object, and
   the dependency's :file on disk.  Please refer to `resolve-dependencies*` for details
   on usage, or use it if you need access to Aether dependency resolution objects."
  [& args]
  (-> (apply resolve-dependencies* args)
    .getRoot
    dependency-graph))

(defn dependency-files
  "Given a dependency graph obtained from `resolve-dependencies`, returns a seq of
   files from the dependencies' metadata."
  [graph]
  (->> graph keys (map (comp :file meta)) (remove nil?)))

(defn- exclusion= [spec1 spec2]
  (let [[dep & opts] (normalize-exclusion-spec spec1)
        [sdep & sopts] (normalize-exclusion-spec spec2)
        om (apply hash-map opts)
        som (apply hash-map sopts)]
    (and (= (group dep)
            (group sdep))
         (= (name dep)
            (name sdep))
         (= (:extension om "*")
            (:extension som "*"))
         (= (:classifier om "*")
            (:classifier som "*"))
         spec2)))

(defn- exclusions-match? [excs sexcs]
  (if-let [ex (first excs)]
    (if-let [match (some (partial exclusion= ex) sexcs)]
      (recur (next excs) (remove #{match} sexcs))
      false)
    (empty? sexcs)))

(defn within?
  "Determines if the first coordinate would be a version in the second
   coordinate. The first coordinate is not allowed to contain a
   version range."
  [[dep version & opts] [sdep sversion & sopts]]
  (let [om (apply hash-map opts)
        som (apply hash-map sopts)]
    (and (= (group dep)
            (group sdep))
         (= (name dep)
            (name sdep))
         (= (:extension om "jar")
            (:extension som "jar"))
         (= (:classifier om)
            (:classifier som))
         (= (:scope om "compile")
            (:scope som "compile"))
         (= (:optional om false)
            (:optional som false))
         (exclusions-match? (:exclusions om) (:exclusions som))
         (or (= version sversion)
             (if-let [[_ ver] (re-find #"^(.*)-SNAPSHOT$" sversion)]
               (re-find (re-pattern (str "^" ver "-\\d+\\.\\d+-\\d+$"))
                        version)
               (let [gsv (GenericVersionScheme.)
                     vc (.parseVersionConstraint gsv sversion)
                     v (.parseVersion gsv version)]
                 (.containsVersion vc v)))))))

(defn dependency-hierarchy
  "Returns a dependency hierarchy based on the provided dependency graph
   (as returned by `resolve-dependencies`) and the coordinates that should
   be the root(s) of the hierarchy.  Siblings are sorted alphabetically."
  [root-coordinates dep-graph]
  (let [root-specs (map (comp dep-spec dependency) root-coordinates)
        hierarchy (for [root (filter
                              #(some (fn [root] (within? % root)) root-specs)
                              (keys dep-graph))]
                    [root (dependency-hierarchy (dep-graph root) dep-graph)])]
    (when (seq hierarchy)
      (into (sorted-map-by #(apply compare (map coordinate-string %&))) hierarchy))))

