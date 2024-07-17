(ns ^:no-wiki org.clojars.evenmoreirrelevance.doubledot.detail
  (:require [clojure.spec.alpha :as spec]
            [potemkin.walk :as walk]
            [clojure.string :as str]
            [clojure.core.protocols]
            [clojure.edn :as edn]))

(def doubledot-literal
  :org.clojars.evenmoreirrelevance.doubledot/l)

(defmacro rex
  ([typ {:as info}]
   (assert (.isAssignableFrom Throwable (resolve typ)))
   (assert (not (.isAssignableFrom clojure.lang.IExceptionInfo (resolve typ))))
   `(let [info# (cast clojure.lang.IPersistentMap ~info)]
      (proxy [~typ clojure.lang.IExceptionInfo] []
        (getData [] info#))))
  ([typ msg {:as info} & more]
   (assert (.isAssignableFrom Throwable (resolve typ)))
   (assert (not (.isAssignableFrom clojure.lang.IExceptionInfo (resolve typ))))
   `(let [msg# ~msg info# (cast clojure.lang.IPersistentMap ~info)]
      (proxy [~typ clojure.lang.IExceptionInfo] [msg# ~@more]
        (getData [] info#)))))

(defn conform!!
  [spec val]
  (let [c (spec/conform spec val)]
    (if-not (spec/invalid? c)
      c
      (throw (rex IllegalArgumentException
                  "Couldn't conform to spec"
                  {:value val :spec spec
                   :explanation (spec/explain-str spec val)})))))

(defn class-edn-read-roundtrips?
  [^Class x]
  (try (= (.getName x)
          (str (edn/read-string (.getName x))))
       (catch Exception _ false)))

(defmacro effects!
  [& body]
  `(do ~@body nil))

(do
  (declare ->MetaBox)
  (deftype MetaBox [m x]
    clojure.lang.IDeref
    (deref [_] x)
    clojure.lang.IObj
    (meta [_] m)
    (withMeta [_ nm] (->MetaBox nm x)))

  (defn metabox
    [x]
    (->MetaBox (meta x) x))

  (defn unmetabox
    [x]
    (if (instance? MetaBox x)
      (with-meta @x (meta x))
      x)))

(spec/def ::nonblank-simple-symbol
  (spec/and simple-symbol? #(not (str/blank? (name %)))))

(spec/def ::pkg->alias
  (spec/map-of ::nonblank-simple-symbol ::nonblank-simple-symbol))

(defonce nothing (Object.))

(defmacro if-val
  {:clj-kondo/lint-as 'clojure.core/if-let}
  [[v [k m]] then else]
  `(let [~v (get ~k ~m nothing)]
     (if-not (identical? nothing ~v)
       ~then
       (let [~v nil]
         ~else))))

(defn merge-self-with*
  [m1 f m & ms]
  (reduce
   (fn merge-with*-step [curr [k v]]
     (assoc curr k
            (if-val [oldv (curr k)]
                    (f k oldv v)
                    v)))
   m1 (apply concat m ms)))

(defn compare-by-prefix-length-alpha
  [^String s1 s2]
  (cond
    (= s1 s2) 0
    (str/starts-with? s1 s2) -1
    (str/starts-with? s2 s1) 1
    :else (.compareTo s1 s2)))

(defonce *ns->
  (atom {::shorthand->package {}
         ::nickname->classname {}}))

(defn parse-shortened
  [strn shorthand->full]
  (when shorthand->full
    (when-first [[short full] (filter #(and (str/starts-with? strn (key %))
                                            (re-matches #"[.].+" (subs strn (count (key %)))))
                                      (subseq shorthand->full > strn))]
      (str full (subs strn (count short))))))

#_(spec/def ::nicks->classes
    (spec/map-of
     (spec/and ::nonblank-simple-symbol #(not (str/index-of (name %) \.)))
     class?))

#_(defn parse-nicknamed
    [strn nick->name]
    (get nick->name strn))

(def end-dot-err-explanation
  "
This is because of potentially unintuitive behaviors with how package qualifiers would need to be resolved,
and due to the necessity to have a special case for nickname resolution.
Use the full symbol instead or, if applicable, replace `(..AClass. ...)` with `(..AClass/new ...)`
or `(new ..AClass ...)`.")

(defn ns->transform
  [ns_]
  (let [ns-name (.getName ^clojure.lang.Namespace ns_)
        ns-> @*ns->
        shorthands (get-in ns-> [::shorthand->package ns-name])
        nicknames (get-in ns-> [::nickname->classname ns-name])]
    (fn [proc]
      (cond
        (str/ends-with? proc ".")
        (throw (rex IllegalArgumentException
                    (str "Refusing to process input because it ends in a dot.\n"
                         end-dot-err-explanation)
                    {:input proc}))
        :else
        (let [out (or #_(parse-nicknamed proc nicknames)
                      (parse-shortened proc shorthands)
                      (throw (rex IllegalArgumentException
                                  "Couldn't find valid shortening/replacement for element."
                                  {:input proc :shorthands shorthands :nicknames nicknames})))]
          (cond (and (not= out proc) (not (class? (ns-resolve ns_ (symbol out)))))
                (throw (rex IllegalArgumentException
                            "Transformed output doesn't resolve to a class."
                            {:output out}))
                :else
                out))))))

(defn read-shortened*
  [walkable-meta-keys transform frm]
  (->> frm
       (walk/prewalk
        (fn [e]
          (let [new-e (cond
                        (or (-> e meta doubledot-literal)
                            (and (seq? e) (= `quote (first e))))
                        (metabox e)
                        (not (symbol? e))
                        e
                        ;; makes this idempotent
                        (simple-symbol? e)
                        (symbol (transform (name e)))
                        :else
                        (symbol (transform (namespace e)) (name e)))]
            (cond-> new-e
              (instance? clojure.lang.IObj new-e)
              (-> (with-meta (meta e))
                  (vary-meta (fn [m]
                               (let [cont #(read-shortened* walkable-meta-keys transform %)]
                                 (reduce #(update % %2 cont) m (case walkable-meta-keys
                                                                 ::all (keys m)
                                                                 walkable-meta-keys))))))
              (and (symbol? e) (not= new-e e))
              (-> (vary-meta assoc doubledot-literal true))))))
       (walk/prewalk unmetabox)))

#_(defn raw-ns-nicknames
  [ns_]
  (get-in @*ns-> [::nickname->classname (.getName ^clojure.lang.Namespace ns_)]))

(defn raw-ns-shorthands
  [ns_] 
  (get-in @*ns-> [::shorthand->package (.getName ^clojure.lang.Namespace ns_)]))