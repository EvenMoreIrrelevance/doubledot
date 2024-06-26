(ns build
  (:require [clojure.tools.build.api :as b]
            [clojure.java.io :as jio])
  (:import [javax.xml.parsers DocumentBuilderFactory]
           [javax.xml.xpath XPathFactory XPathConstants]
           [org.w3c.dom Node NodeList]))

(def pom
  (with-open [r (jio/input-stream "pom.xml")]
    (-> (DocumentBuilderFactory/newInstance)
        (.newDocumentBuilder)
        (.parse r))))

NodeList

(let [xpaths (XPathFactory/newInstance)]
  (defn xpath-select
    [query doc]
    (let [^NodeList nodes (.evaluate (.newXPath xpaths) ^String query doc XPathConstants/NODESET)]
      (mapv #(.item nodes %)
            (range (.getLength nodes))))))

(def lib
  (-> (xpath-select "/project/name/text()" pom)
      (first) (Node/.getTextContent) 
      (keyword)))

(def version
  (-> (xpath-select "/project/version/text()" pom)
      (first) (Node/.getTextContent)))

(def class-dir "target/classes")
(def jar-file (format "target/%s-%s.jar" (name lib) version))

;; delay to defer side effects (artifact downloads)
(def basis (delay (b/create-basis {:project "deps.edn"})))

(defn- jar-opts [opts]
  (assoc opts
         :lib lib :version version
         :jar-file (format "target/%s-%s.jar" lib version)
         :scm {:tag (str "v" version)}
         :basis @basis
         :class-dir class-dir
         :target "target"
         :src-dirs ["src"]))

(defn clean [_]
  (b/delete {:path "target"}))

(defn jar [_]
  (b/write-pom (jar-opts {}))
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir})
  (b/jar {:class-dir class-dir
          :jar-file jar-file}))