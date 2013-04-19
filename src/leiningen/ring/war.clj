(ns leiningen.ring.war
  (:require [leiningen.compile :as compile]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [leinjacker.deps :as deps]
            [leiningen.classpath :refer (get-classpath-string)])
  (:use [clojure.data.xml :only [sexp-as-element indent-str]]
        leiningen.ring.util
        [leinjacker.eval :only (eval-in-project)])
  (:import [java.util.jar Manifest
                          JarEntry
                          JarOutputStream]
           [java.io BufferedOutputStream
                    FileOutputStream
                    ByteArrayInputStream]
           [org.eclipse.jetty.util.resource Resource]))

(defn default-war-name [project]
  (or (get-in project [:ring :war-name])
      (str (:name project) "-" (:version project) ".war")))

(defn war-file-path [project war-name]
  (let [target-dir (or (:target-dir project) (:target-path project))]
    (.mkdirs (io/file target-dir))
    (str target-dir "/" war-name)))

(defn skip-file? [project war-path file]
  (or (re-find #"^\.?#" (.getName file))
      (re-find #"~$" (.getName file))
      (some #(re-find % war-path)
            (get-in project [:ring :war-exclusions] [#"(^|/)\."]))))

(defn- to-byte-stream [^String s]
  (ByteArrayInputStream. (.getBytes s)))

(def default-ring-manifest
  {"Created-By"       "Leiningen Ring Plugin"
   "Built-By"         (System/getProperty "user.name")
   "Build-Jdk"        (System/getProperty "java.version")})

(defn make-manifest [user-manifest]
  (Manifest.
   (to-byte-stream
    (reduce
     (fn [accumulated-manifest [k v]]
       (str accumulated-manifest "\n" k ": " v))
     "Manifest-Version: 1.0"
     (merge default-ring-manifest user-manifest)))))

(defn default-servlet-class [project]
  (let [handler-sym (get-in project [:ring :handler])
        ns-parts    (-> (namespace handler-sym)
                        (string/replace "-" "_")
                        (string/split #"\.")
                        (butlast)
                        (vec)
                        (conj "servlet" (name handler-sym)))]
    (string/join "." ns-parts)))

(defn servlet-class [project]
  (or (get-in project [:ring :servlet-class])
      (default-servlet-class project)))

(defn servlet-ns [project]
  (-> (servlet-class project)
      (string/replace "_" "-")))

(defn servlet-name [project]
  (or (get-in project [:ring :servlet-name])
      (str (get-in project [:ring :handler])
           " servlet")))

(defn has-listener? [project]
  (let [ring-options (:ring project)]
    (or (contains? ring-options :init)
        (contains? ring-options :destroy))))

(defn default-listener-class [project]
  (let [listener-sym (or (get-in project [:ring :init])
                         (get-in project [:ring :destroy]))
        ns-parts     (-> (namespace listener-sym)
                         (string/replace "-" "_")
                         (string/split #"\.")
                         (butlast)
                         (vec)
                         (conj "listener"))]
    (string/join "." ns-parts)))

(defn listener-class [project]
  (or (get-in project [:ring :listener-class])
      (default-listener-class project)))

(defn listener-ns [project]
  (-> (listener-class project)
      (string/replace "_" "-")))

(defn url-pattern [project]
  (or (get-in project [:ring :url-pattern])
      "/*"))

(defn war-resources-path [project]
  (:war-resources-path project "war-resources"))

(defn get-servlet-mappings [project]
  (distinct
    (remove #(or (nil? %)
                 (.startsWith % "/WEB-INF")
                 (.startsWith % "/META-INF")
                 (.startsWith % "/.DS_Store"))
            (apply concat
                   (map #(str \/ (.getName %) (when (.isDirectory %) "/*"))
                        (.listFiles
                          (io/as-file
                            (war-resources-path project))))
                   (for [x (string/split (get-classpath-string project) #":")
                         :let [file (io/as-file x)
                               resource (Resource/newResource file)
                               filename (.getName file)]
                         :when (.endsWith filename ".jar")]
                     (let [meta-inf-resource (Resource/newResource
                                               (str "jar:file:"
                                                    (.getCanonicalPath file)
                                                    "!/META-INF/resources"))]
                       (when (.exists meta-inf-resource)
                         (map #(let [dir? (.isDirectory
                                            (Resource/newResource
                                              (str meta-inf-resource %)))]
                                 (str \/ % (when dir? \*)))
                              (.list meta-inf-resource)))))))))

(defn make-web-xml [project]
  (let [ring-options (:ring project)]
    (if (contains? ring-options :web-xml)
      (slurp (:web-xml ring-options))
      (indent-str
        (sexp-as-element
          [:web-app
           (if (has-listener? project)
             [:listener
              [:listener-class (listener-class project)]])
           (when-let [mappings (get-servlet-mappings project)]
             (map (fn [mapping] [:servlet-mapping
                                 [:servlet-name "default"]
                                 [:url-pattern mapping]]) mappings))
           [:servlet
            [:servlet-name  (servlet-name project)]
            [:servlet-class (servlet-class project)]]
           [:servlet-mapping
            [:servlet-name (servlet-name project)]
            [:url-pattern (url-pattern project)]]])))))

(defn generate-handler [project handler-sym]
  (if (get-in project [:ring :servlet-path-info?] true)
    `(fn [request#]
       (~handler-sym
         (assoc request#
           :path-info (.getPathInfo (:servlet-request request#))
           :context   (.getContextPath (:servlet-request request#)))))
    handler-sym))

(defn compile-servlet [project]
  (let [handler-sym (get-in project [:ring :handler])]
    (if (coll? handler-sym)
      (doseq [handler handler-sym]
        (compile-servlet (assoc-in project [:ring :handler] handler)))
      (let [handler-ns  (symbol (namespace handler-sym))
            servlet-ns  (symbol (servlet-ns project))]
        (compile-form project servlet-ns
          `(do (ns ~servlet-ns
                 (:require ring.util.servlet ~handler-ns)
                 (:gen-class :extends javax.servlet.http.HttpServlet))
               (ring.util.servlet/defservice
                 ~(generate-handler project handler-sym))))))))

(defn compile-listener [project]
  (let [init-sym    (get-in project [:ring :init])
        destroy-sym (get-in project [:ring :destroy])
        init-ns     (and init-sym    (symbol (namespace init-sym)))
        destroy-ns  (and destroy-sym (symbol (namespace destroy-sym)))
        project-ns  (symbol (listener-ns project))]
    (compile-form project project-ns
      `(do (ns ~project-ns
             (:require ~@(set (remove nil? [init-ns destroy-ns])))
             (:gen-class :implements [javax.servlet.ServletContextListener]))
           ~(let [servlet-context-event (gensym)]
              `(do
                 (defn ~'-contextInitialized [this# ~servlet-context-event]
                   ~(if init-sym
                      `(~init-sym)))
                 (defn ~'-contextDestroyed [this# ~servlet-context-event]
                   ~(if destroy-sym
                      `(~destroy-sym)))))))))

(defn create-war [project file-path]
  (-> (FileOutputStream. file-path)
      (BufferedOutputStream.)
      (JarOutputStream. (make-manifest (:manifest project)))))

(defn write-entry [war war-path entry]
  (.putNextEntry war (JarEntry. war-path))
  (io/copy entry war))

(defn str-entry [war war-path content]
  (write-entry war war-path (to-byte-stream content)))

(defn in-war-path [war-path root file]
  (str war-path
       (-> (.toURI (io/file root))
           (.relativize (.toURI file))
           (.getPath))))

(defn file-entry [war project war-path file]
  (when (and (.exists file)
             (.isFile file)
             (not (skip-file? project war-path file)))
    (write-entry war war-path file)))

(defn dir-entry [war project war-root dir-path]
  (doseq [file (file-seq (io/file dir-path))]
    (let [war-path (in-war-path war-root dir-path file)]
      (file-entry war project war-path file))))

(defn web-xml-in-resources? [project]
  (.exists (io/as-file
             (str (war-resources-path project) "/WEB-INF/web.xml"))))

(defn write-war [project war-path]
  (with-open [war-stream (create-war project war-path)]
    (if-not (web-xml-in-resources? project)
      (str-entry war-stream "WEB-INF/web.xml" (make-web-xml project))
      (println "Using included WEB-INF/web.xml"))
    (doto war-stream
      (dir-entry project "WEB-INF/classes/" (:compile-path project)))
    (doseq [path (distinct (concat [(:source-path project)] (:source-paths project)
                                   [(:resources-path project)] (:resource-paths project)))
            :when path]
      (dir-entry war-stream project "WEB-INF/classes/" path))
    (dir-entry war-stream project "" (war-resources-path project))
    war-stream))

(defn add-servlet-dep [project]
  (deps/add-if-missing project '[ring/ring-servlet "1.1.6"]))

(defn war
  "Create a $PROJECT-$VERSION.war file."
  ([project]
     (war project (default-war-name project)))
  ([project war-name]
     (ensure-handler-set! project)
     (let [project (add-servlet-dep project)
           result  (compile/compile project)]
       (when-not (and (number? result) (pos? result))
         (let [war-path (war-file-path project war-name)]
           (compile-servlet project)
           (if (has-listener? project)
             (compile-listener project))
           (write-war project war-path)
           (println "Created" war-path)
           war-path)))))
