(ns leiningen.nix
  (:require
   [clojure.java.shell :refer [sh]]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [leiningen.core.classpath :as cp]
   [leiningen.core.eval :as eval]
   [leiningen.core.main :as main]
   [leiningen.core.project :as prj]
   [leiningen.deps :as deps]
   [leiningen.trampoline :as tramp]
   [clojure.pprint :refer [pprint with-pprint-dispatch code-dispatch]])
  #_([webnf.base :refer [pr-cls]]
     [cemerick.pomegranate.aether :as aether]
     [clojure.pprint :refer [pprint]]))

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

(defn init-trampoline [project task-name args]
  (binding [tramp/*trampoline?* true]
    (main/apply-task (main/lookup-alias task-name project)
                     (-> (assoc project :eval-in :trampoline)
                         (vary-meta update-in [:without-profiles] assoc
                                    :eval-in :trampoline))
                     args))
  (let [forms (map rest @eval/trampoline-forms)]
    {:init-form (concat '(do)
                        (map first forms)
                        (mapcat rest forms))
     :java-args (:jvm-opts @eval/trampoline-project)}))

(defn pprint-code [o indent]
  (str/replace
   (with-out-str
     (with-pprint-dispatch
       code-dispatch
       (pprint o)))
   #"\n" (apply str "\n" (repeat indent \space))))

(defn render-classpath [project indent]
  (concat
   ["["]
   (interleave
    (repeat (apply str \newline (repeat (+ 2 indent) \space)))
    (for [p (concat
             (:source-paths project)
             (:resource-paths project)
             [(:compile-path project)]
             (for [dep (cp/resolve-dependencies :dependencies project)]
               (.getAbsolutePath ^java.io.File dep)))
          :when (.exists (io/file p))]
      p))
   [\newline (apply str (repeat indent \space)) "]"]))

(defn project-name [project]
  (or (:deploy-name project)
      (name (:name project))))

(defn render-task [project task-name args indent]
  (let [{:keys [init-form java-args]} (init-trampoline project task-name args)
        indent-s (apply str (repeat (+ 2 indent) \space))]
    (concat
     ["{" \newline
      indent-s "name = \"" (project-name project) "\";\n"
      indent-s "version = \"" (:version project) "\";\n"
      indent-s "initForm = ''\n"
      (apply str (repeat (+ 4 indent) \space))
      (pprint-code init-form (+ 4 indent)) \newline
      indent-s "'';\n"
      indent-s "javaArgs = " (pr-str java-args) ";\n"
      indent-s "classpath = "]
     (render-classpath project (+ 2 indent))
     [";\n" (apply str (repeat indent \space)) "}"])))

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
  (let [d (io/file (:target-path project) "lein-nix")]
    (.mkdirs d)
    (str d)))

(defn nix-classpath [project]
  (let [tp (target-path! project)]
    (render-target! project "classpath.nix" (render-classpath project 0))
    (nix-build! tp
                (str "writeText \"" (project-name project)
                     "-classpath\" (lib.concatStringsSep \"\n\" (import ./classpath.nix))")
                "classpath-link")))

(defn nix-task [project task-name & args]
  (let [tp (target-path! project)]
    (render-target! project "descriptor.nix" (render-task project task-name args 0))
    (render-target! project "classpath.nix" (render-classpath project 0))
    (io/copy
     (io/reader (io/resource "lein-nix/task.nix"))
     (io/file tp "task.nix"))
    (io/copy
     (io/reader (io/resource "lein-nix/start.tmpl.sh"))
     (io/file tp "start.tmpl.sh"))
    (nix-build-pkg! tp "./task.nix" "task-link")))

(defn nix [project task & args]
  {:subtasks [#'nix-task #'nix-classpath]}
  (let [out (case task
              "task" (apply nix-task project args)
              "classpath" (apply nix-classpath project args))]
    (.flush *err*)
    (.write *out* out)
    (.flush *out*)))

#_(defn nix [project task-name & args]
    (let [{:keys [init-form java-args]} (init-trampoline project task-name args)]
      (println "{")
      (println (str "  version = \"" (:version project) "\";"))
      (println "  initForm = ''")
      (println "   " (pprint-code init-form 4))
      (println "  '';")
      (println (str "  javaArgs = " (pr-str java-args) ";"))
      (println "  classpath = ''")
      (doseq [p (concat
                 (:source-paths project)
                 (:resource-paths project)
                 [(:compile-path project)]
                 (for [dep (cp/resolve-dependencies :dependencies project)]
                   (.getAbsolutePath dep)))]
        (when (.exists (io/file p))
          (println "    ${" p "}")))
      (println "  '';")
      (println "}")))
