(ns leiningen.nix
  (:require
   [clojure.java.shell :refer [sh]]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [leiningen.jar :as jar]
   [leiningen.core.classpath :as cp]
   [leiningen.core.eval :as eval]
   [leiningen.core.main :as main]
   [leiningen.core.project :as prj]
   [leiningen.deps :as deps]
   [leiningen.trampoline :as tramp]
   [clojure.pprint :refer [pprint with-pprint-dispatch code-dispatch]]
   [nix.data :as nd]
   #_([webnf.base :refer [pr-cls]]
      [cemerick.pomegranate.aether :as aether]
      [clojure.pprint :refer [pprint]])))

#_(defn flatten-hierarchy [h]
    (when h
      (concat (keys h) (mapcat flatten-hierarchy (vals h)))))

#_(defn resolve-deps [prj dep-key]
    (aether/resolve-dependencies*
     :retrieve true
     :repositories (:repositories prj)
     :coordinates (get prj dep-key)))

#_(defn dep-descriptors [prj dep-key]
    (flatten-hierarchy
     (aether/dependency-hierarchy
      (get prj dep-key)
      (resolve-deps prj dep-key))))

#_(defn resolve-arts [prj dep-key]
    (aether/resolve-artifacts
     :repositories (:repositories prj)
     :coordinates (get prj dep-key)))

(defn pprint-code [o]
  (with-out-str
    (with-pprint-dispatch
      code-dispatch
      (pprint o))))

(def devnull (java.io.PrintWriter.
              (io/writer "/dev/null")))

(defn project-jar [project]
  (let [res (binding [*out* *err*]
              (jar/jar (assoc project :auto-clean false)))]
    (when-not (= 1 (count res))
      (throw (ex-info "Multiple Artifacts" {:res res :project project})))
    (second (first res))))

(defn dev-paths [project]
  (map #(str (.getAbsolutePath (io/file %)) "/")
       (concat (:source-paths project)
               (:resource-paths project))))

(defn project-dep-paths [project]
  (for [p (cp/resolve-dependencies :dependencies project)
        :when (.exists (io/file p))]
    (nd/path p)))

(defn project-jar-path [project]
  (nd/path (project-jar project)))

(defn nix-prefetch! [file]
  (let [{:keys [exit out err] :as res} (sh "nix-prefetch-url" (str "file://" (.getAbsolutePath file)))]
    (.println *err* (str "nix-prefetch-url file://" (.getAbsolutePath file) " => " (pr-str res)))
    (if (zero? exit)
      (str/trim out)
      (throw (ex-info err res)))))

(defn render-deps [project]
  (reduce (fn [deps [coord version]]
            #_(assoc-in deps [(or (namespace coord) (name coord))
                              (name coord)]
                        version
                        #_{:sha256 (nix-prefetch! (:file (meta dep)))})
            (conj deps [(or (namespace coord) (name coord))
                        (name coord)
                        version]))
          [] (:dependencies project) #_(cp/get-dependencies :dependencies project)))

(defn render-classpath
  ([project] (render-classpath project false))
  ([project mutable-src]
   (vec (nd/inc-indent
         (interleave
          (repeat (nd/fragment (cons "\n" nd/*indent*)))
          (if mutable-src
            (concat
             (dev-paths project)
             ;; FIXME only override depencencies of project
             (apply concat (vals mutable-src))
             (project-dep-paths project))
            (cons
             (project-jar-path project)
             (project-dep-paths project))))))))

(defn init-trampoline [project task-name args]
  (locking #'tramp/*trampoline?*
    (binding [tramp/*trampoline?* true]
      (reset! eval/trampoline-project nil)
      (reset! eval/trampoline-forms [])
      (reset! eval/trampoline-profiles [])
      (main/apply-task (main/lookup-alias task-name project)
                       (-> (assoc project :eval-in :trampoline)
                           (vary-meta update-in [:without-profiles] assoc
                                      :eval-in :trampoline))
                       args))
    (let [forms (map rest @eval/trampoline-forms)]
      {:init-form (concat '(do)
                          (map first forms)
                          (mapcat rest forms))
       :classpath (render-classpath @eval/trampoline-project)
       :java-args (:jvm-opts @eval/trampoline-project)})))

(defn project-name [project]
  (or (:deploy-name project)
      (name (:name project))))

(defn render-task [project task-name args]
  (let [{:keys [init-form java-args classpath]} (init-trampoline project task-name args)]
    {:name (project-name project)
     :version (:version project)
     :initForm (nd/ppstr (pprint-code init-form))
     :javaArgs (vec java-args)
     :classpath classpath}))

(defn render-package [project prj-keys]
  {:name (project-name project)
   :version (:version project)
   :package (nd/ppstr (pr-str (select-keys project prj-keys)))})

(defn nix-build! [wd eval-str out-link]
  (let [args ["-E" (str "with import <nixpkgs> {}; " eval-str)
              :dir (str wd)]
        {:keys [exit out err] :as res}
        (apply sh "nix-build"
               (if out-link
                 (list* "--out-link" out-link args)
                 (cons  "--no-out-link" args)))]
    (.println *err* (str "Finished `" (apply str (interpose " " (list* "nix-build"
                                                                       (if out-link
                                                                         (list* "--out-link" out-link args)
                                                                         (cons  "--no-out-link" args)))))
                         "` => " (pr-str res)))
    (if (zero? exit)
      out
      (throw (ex-info err res)))))

(defn nix-build-pkg! [wd pkg out-link]
  (nix-build! wd (str "callPackage " pkg " {}") out-link))

(defn render-target! [project path str-seq]
  (with-open [w (io/writer (io/file (:target-path project)
                                    "lein-nix" path))]
    (doseq [s str-seq]
      (.write w (str s)))))

(defn target-path! [project]
  (let [d (io/file (:target-path project) "lein-nix")
        res (.mkdirs d)]
    (.write *err* (str "MKDIR " d " => " res "\n"))
    (str d)))

(defn nix-classpath
  ([project] (nix-classpath project false))
  ([project mutable-src]
   (let [tp (target-path! project)]
     (render-target! project "classpath.nix" (nd/render (render-classpath project mutable-src)))
     (nix-build! tp
                 (str "writeText \"" (project-name project)
                      "-classpath\" (lib.concatStringsSep \"\n\" (import ./classpath.nix))")
                 "classpath-link"))))

(defn nix-package [project & prj-keys]
  (let [tp (target-path! project)]
    (render-target! project "descriptor.nix"
                    (nd/render (render-package project (map keyword prj-keys))))
    (render-target! project "classpath.nix"
                    (nd/render (render-classpath project)))
    (io/copy
     (io/reader (io/resource "lein-nix/package.nix"))
     (io/file tp "package.nix"))
    (nix-build-pkg! tp "./package.nix" "package-link")))

(defn nix-task [project task-name & args]
  (let [tp (target-path! project)]
    (render-target! project "descriptor.nix" (nd/render (render-task project task-name args)))
    (io/copy
     (io/reader (io/resource "lein-nix/task.nix"))
     (io/file tp "task.nix"))
    (io/copy
     (io/reader (io/resource "lein-nix/start.tmpl.sh"))
     (io/file tp "start.tmpl.sh"))
    (nix-build-pkg! tp "./task.nix" "task-link")))

(defn nix-descriptor [project]
  (target-path! project)
  (render-target! project "deps.nix"
                  (nd/render (render-deps project))))

;; nix-build support

(defn nix-build-classpath [project target]
  (spit target (nd/render (render-classpath project))))

(defn nix [project task & args]
  {:subtasks [#'nix-task #'nix-classpath #'nix-package]}
  (let [out (apply (case task
                     "task" nix-task
                     "classpath" nix-classpath
                     "package" nix-package
                     "build-classpath" nix-build-classpath
                     "dev-classpath" #(nix-classpath %1 (read-string %2))
                     "descriptor" nix-descriptor)
                   project args)]
    (.write *err* (str "LEIN NIX CALLED " (apply pr-str task args) "\n"))
    (.flush *err*)
    (.write *out* out)
    (.flush *out*)))

(comment
  (def prj (prj/read "/home/herwig/src/cc.bendlas.net/project.clj"))
  (def prj (prj/read "/home/herwig/src/hdnews.bendlas.net/project.clj"))
  (def prj (prj/read "/home/herwig/checkout/bendlas.net/lib/davstore/project.clj"))

  (nix-descriptor prj)
  )
