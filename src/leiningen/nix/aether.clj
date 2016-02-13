(ns leiningen.nix.aether
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [cemerick.pomegranate.aether :as aether])
  (:import  (org.sonatype.aether.resolution
             ArtifactDescriptorRequest
             MetadataRequest)
            (org.sonatype.aether.util.metadata
             DefaultMetadata)
            (org.sonatype.aether.metadata
             Metadata$Nature)))

(defn run-with-session [{:keys [repositories coordinates files retrieve local-repo
                                transfer-listener offline? proxy mirrors repository-session-fn]}
                        fk]
  (let [repositories @#'aether/maven-central
        system (@#'aether/repository-system)
        mirror-selector-fn (memoize (partial @#'aether/mirror-selector-fn nil #_mirrors))
        mirror-selector (@#'aether/mirror-selector mirror-selector-fn nil #_proxy)
        session (@#'aether/repository-session
                 {:repository-system system
                  ;; :local-repo local-repo
                  ;; :offline? offline?
                  ;; :transfer-listener transfer-listener
                  :mirror-selector mirror-selector})
        repositories (vec (map #(let [repo (@#'aether/make-repository % nil #_proxy)]
                                  (-> session
                                      (.getMirrorSelector)
                                      (.getMirror repo)
                                      (or repo)))
                               repositories))]
    (fk system session repositories)))

(defn artifact-descriptors [& {:keys [coordinates] :as cfg}]
  (run-with-session
   cfg (fn [system session repositories]
         (doall
          (for [coo coordinates
                :let [dep (@#'aether/artifact coo)]]
            #_(if-let [local-file (get files %)]
                (-> (artifact %)
                    (.setProperties
                     {ArtifactProperties/LOCAL_PATH
                      (.getPath (io/file local-file))}))
                (artifact %))
            (.readArtifactDescriptor
             system session (ArtifactDescriptorRequest.
                             dep repositories nil)))))))


(defn artifact-descriptors [& {:keys [coordinates] :as cfg}]
  (run-with-session
   cfg (fn [system session repositories]
         (doall
          (for [coo coordinates
                :let [dep (@#'aether/artifact coo)]]
            (.readArtifactDescriptor
             system session (ArtifactDescriptorRequest.
                             dep repositories nil)))))))

(defn metadata [& {:keys [coordinates] :as cfg}]
  (run-with-session
   cfg (fn [system session repositories]
         (.resolveMetadata
          system session 
          (for [[group-artifact version
                 & {:keys [classifier extension]}] coordinates
                :let [artifact (name group-artifact)
                      group (or (namespace group-artifact) artifact)]]
            (MetadataRequest.
             (DefaultMetadata.
              group artifact nil nil
              Metadata$Nature/RELEASE_OR_SNAPSHOT)))))))

(comment

  (def ad
    (first
     (artifact-descriptors
      :coordinates
      '[[org.eclipse.jetty/jetty-webapp "9.3.6.v20151106"]])))

  (.getManagedDependencies ad)
  (.getDependencies ad)
  (.getProperties ad)

  (def am
    (metadata
     :coordinates
     '[[org.eclipse.jetty/jetty-webapp "9.3.6.v20151106"]]))
  
  )
