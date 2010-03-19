(ns leiningen.repl
  "Starts a REPL using the project's classpath."
  (:use [leiningen.compile :only [eval-in-project find-lib-jars]]
        [clojure.contrib.seq-utils :only [flatten]])
  (:require [clojure.main])
  (:import [java.net URL URLClassLoader]
           [java.io File FilenameFilter]))

(def separator (System/getProperty "file.separator"))

(defn file->url [file]
  "Returns the file:/ URL for the provided java.io.File or filename"
  (.toURL (File. (str file))))

(defn url->file [url]
  "Returns the java.io.File referenced by the provided file:/ URL"
  (File. (.toURI url)))

(defn files-in-dir
  ([dir]
     (let [dirFile (File. dir)]
       (.listFiles dirFile)))
  ([dir suffix]
     (let [dirFile (File. dir)
	   fileFilter (proxy [FilenameFilter] []
                        (accept [dir name] (.endsWith name suffix)))]
       (.listFiles dirFile fileFilter))))

(defn make-classpath [& directory-names]
  (map file->url (flatten directory-names)))

(defn format-path [path]
  (apply str (interpose separator (map url->file path))))

(defn reflectively-invoke-java-method [java-class method-name & args]
  (let [method (.getMethod java-class method-name (into-array (map class args)))]
    (.invoke method nil (into-array Object args))))

(defn invoke-fn-in-new-classloader
  "Creates a new classLoader with the new classpath and then invokes the specified function"
  [project-classpath namespace-name symbol-name & args]
  (let [classLoader (URLClassLoader. (into-array project-classpath) (.getParent (ClassLoader/getSystemClassLoader)))
	_ (.setContextClassLoader (Thread/currentThread) classLoader)
	rt (Class/forName "clojure.lang.RT" true classLoader)
        _ (reflectively-invoke-java-method rt "load" "clojure.main")
	fn-var (reflectively-invoke-java-method rt "var" namespace-name symbol-name)]
    (System/setProperty "java.class.path" (format-path project-classpath))
    (.applyTo fn-var args)))

(defn invoke-method-in-new-classloader
  [project-classpath class-name method-name & args]
  (let [class-loader (URLClassLoader. (into-array project-classpath) (.getParent (ClassLoader/getSystemClassLoader)))
	_ (.setContextClassLoader (Thread/currentThread) class-loader)
	target (.loadClass classLoader class-name)
	method (.getMethod target method-name (into-array Class (map class args)))]
    (System/setProperty "java.class.path" (format-path project-classpath))
    (reflectively-invoke-java-method target method args)))

; (invoke-method-in-new-classloader "clojure.main" "main" (make-array String 0))

(defn repl
  [project & args]
  (let [project-classpath
	(make-classpath (remove nil? [(:source-path project)
				      (:test-path project)
				      (:compile-path project)
				      (:resources-path project)])
			(map str (find-lib-jars project)))]
    (invoke-fn-in-new-classloader project-classpath "clojure.main" "repl")))
