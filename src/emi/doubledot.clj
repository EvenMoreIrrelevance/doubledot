(ns emi.doubledot
  (:require [clojure.spec.alpha :as spec]
            [clojure.walk :as walk]
            [clojure.string :as str]
            [clojure.core.protocols]
            [clojure.edn :as edn]))

(defmacro ^:private rex
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

(defn ^:private conform!!
  [spec val]
  (let [c (spec/conform spec val)]
    (if-not (spec/invalid? c)
      c
      (throw (rex IllegalArgumentException
                  "Couldn't conform to spec"
                  {:value val :spec spec
                   :explanation (spec/explain-str spec val)})))))

(defn class-edn-read-roundtrips?
  [x]
  (try (= (Class/.getName x)
          (str (edn/read-string (Class/.getName x))))
       (catch Exception _ false)))

(defmacro ^:private effects!
  [& body]
  `(do ~@body nil))

(do
  (declare ->MetaBox)
  (deftype ^:private MetaBox [m x]
    clojure.lang.IDeref
    (deref [_] x)
    clojure.lang.IObj
    (meta [_] m)
    (withMeta [_ nm] (->MetaBox nm x)))

  (defn ^:private metabox
    [x]
    (->MetaBox (meta x) x))

  (defn ^:private unmetabox
    [x]
    (if (instance? MetaBox x)
      (with-meta @x (meta x))
      x)))

(spec/def ::nonblank-simple-symbol
  (spec/and simple-symbol? #(not (str/blank? (name %)))))

(spec/def ::pkg->alias
  (spec/map-of ::nonblank-simple-symbol ::nonblank-simple-symbol))

(defonce ^:private nothing (Object.))

(defmacro ^:private if-val
  {:clj-kondo/lint-as 'clojure.core/if-let}
  [[v [k m]] then else]
  `(let [~v (get ~k ~m nothing)]
     (if-not (identical? nothing ~v)
       ~then
       (let [~v nil]
         ~else))))

(defn ^:private merge-self-with*
  [m1 f m & ms]
  (reduce
   (fn merge-with*-step [curr [k v]]
     (assoc curr k
            (if-val [oldv (curr k)]
                    (f k oldv v)
                    v)))
   m1 (apply concat m ms)))

(defn ^:private compare-by-prefix-length-alpha
  [s1 s2]
  (cond
    (= s1 s2) 0
    (str/starts-with? s1 s2) -1
    (str/starts-with? s2 s1) 1
    :else (String/.compareTo s1 s2)))

(defonce ^:private *ns->
  (atom {::shorthand->package {}
         ::nickname->classname {}}))


(defn ^:private parse-shortened
  [strn shorthand->full]
  (when-first [[short full] (filter #(and (str/starts-with? strn (key %))
                                          (re-matches #"[.].+" (subs strn (count (key %)))))
                                    (subseq shorthand->full > strn))]
    (str full (subs strn (count short)))))

(spec/def ::nicks->classes
  (spec/map-of
   (spec/and ::nonblank-simple-symbol #(not (str/index-of (name %) \.)))
   class?))

(defn ^:private parse-nicknamed
  [strn nick->name]
  (get nick->name strn))

(def end-dot-err-explanation
  "
This is because of potentially unintuitive behaviors with how package qualifiers would need to be resolved,
and due to the necessity to have a special case for nickname resolution.
Use the full symbol instead or, if applicable, replace `(..AClass. ...)` with `(..AClass/new ...)`
or `(new ..AClass ...)`.")

(defn ^:private
  ns->transform
  [ns_]
  (let [ns-name (clojure.lang.Namespace/.getName ns_)
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
        (let [out (or (parse-nicknamed proc nicknames)
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

(defn ^:private read-shortened*
  [walkable-meta-keys transform frm]
  (->> frm
       (walk/prewalk
        (fn [e]
          (let [new-e (cond
                        (or (-> e meta ::l) (and (seq? e) (= 'quote (first e))))
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
              (-> (vary-meta assoc ::l true))))))
       (walk/prewalk unmetabox)))

::PUBLIC-API

(defn nicknames
  "Sets nicknames for the given classes.
   The classes must belong to a package and must `pr` to a simple-symbol."
  ([{:as nicks->classes}]
   (nicknames *ns* nicks->classes))
  ([ns_ {:as nicks->classes}]
   (effects!
    (let [ns-name (clojure.lang.Namespace/.getName ns_)]
      (swap! *ns-> update-in [::nickname->classname ns-name]
             (fn [ns-nicknames->classnames]
               (-> (or ns-nicknames->classnames {})
                   (merge-self-with*
                    (fn [k o n]
                      (if (or (nil? o) (= o n))
                        n
                        (throw (rex IllegalStateException
                                    "Nickname already in use"
                                    {:nickname k :old-target o :new-target n}))))
                    (-> (conform!! ::nicks->classes nicks->classes)
                        (update-keys name)
                        (update-vals #(cond
                                        (not (str/includes? (Class/.getName %) "."))
                                        (throw (rex IllegalArgumentException
                                                    "Classes which aren't package-qualified are disallowed."
                                                    {:input %}))
                                        (not (class-edn-read-roundtrips? %))
                                        (throw (rex IllegalArgumentException
                                                    "Classname doesn't read as a simple-symbol."
                                                    {:input %}))
                                        :else (Class/.getName %))))))))))))

(defn unnicknames
  ([[:as more]] (unnicknames *ns* more))
  ([ns_ [:as more]]
   (effects!
    (swap! *ns->
           update-in [::nickname->classname (clojure.lang.Namespace/.getName ns_)]
           #(reduce dissoc % %2) (map clojure.lang.Symbol/.getName more)))))

(defn ns-nicknames
  "Gets the nicknames in effect in `ns_`.
   Note that you will get a fresh map every time you call this."
  [ns_]
  (some->> (get-in @*ns-> [::nickname->classname (clojure.lang.Namespace/.getName ns_)])
           (into {} (map #(mapv symbol %)))))

(defn shorthands
  "Defines shorthands in `ns_` (default *ns*). 
   Fails atomically if it would override a shorthand."
  ([{:as shorthand->package}] (shorthands *ns* shorthand->package))
  ([ns_ {:as shorthand->package}]
   (effects!
    (let [ns-sym (clojure.lang.Namespace/.getName ns_)]
      (swap! *ns-> update-in [::shorthand->package ns-sym]
             (fn [ns-shorthand->package]
               (-> (or ns-shorthand->package (sorted-map-by compare-by-prefix-length-alpha))
                   (merge-self-with* (fn [k o n]
                                       (if (= o n)
                                         n
                                         (throw (rex IllegalStateException
                                                     "Shorthand already in use"
                                                     {:shorthand k
                                                      :old-target o
                                                      :new-target n}))))
                                     (-> (conform!! ::pkg->alias shorthand->package)
                                         (update-vals name)
                                         (update-keys name))))))))))

(defn unshorthand
  "Removes shorthands in `ns` (default *ns*). Will ignore nonexisting shorthands."
  ([more] (unshorthand *ns* more))
  ([ns_ [& more]]
   (effects!
    (swap! *ns-> update-in [::shorthand->package (clojure.lang.Namespace/.getName ns_)]
           #(reduce dissoc % %2) (map clojure.lang.Symbol/.getName more)))))

(defn ns-shorthands
  "Gets the shorthands for the `ns_`. 
   Note that those are kept around as strings for perf, 
   so you get a fresh map every time you call this."
  [ns_]
  (some->> (get-in @*ns-> [::shorthand->package (clojure.lang.Namespace/.getName ns_)])
           (into {} (map #(mapv symbol %)))))

(defn read-shortened
  "The reader for the `#+!` reader-macro.

   Walks without macroexpanding, and processes simple-symbols beginning by `..`
   and symbols whose `ns` begins with `..` by substituting shorthands for package
   qualifications and nicknames for classes as defined in the current ns.
   Processing happens in lists, maps and vecs too, as well as all meta.

   Adding ^::l to anything's meta will spare that form from processing, 
   and quoted forms are spared too. 
   However, no further attempts are made to be nicer at this stage,
   and note that due to readers being processed in post-order as opposed to pre-order,
   a #+! reader macro in an otherwise-spared subform will be processed anyway.

   This will break if all the following are true:
   - a symbol is fed to a macro/special form which doesn't evaluate it;
   - the symbol is shorthandable;
   - the macro doesn't treat the symbol as a var/class name 
     (which is the intended use case for shorthanding...).

   An immediate example is `(let [..my-short.Class 3] nil) ;=> compile time error`, 
   but the `..` prefix considered distinctive enough and the case is deemed obscure enough 
   to consider this worthwhile, especially since this is a reader-macro
   where the problem should immediately be apparent.
   
   Processing works as follows: If the ns/name ends with `.`, then an error is thrown(1).
   Then, the prefix `..` is elided, and then an attempt is made to replace
   the whole namespace or name with the qualified name of the class with that nickname.
   If that fails, the longest shorthand which is a proper prefix of the ns/name
   which ends in a dot minus the dot is replaced by its long hand form. If that also fails,
   an error is thrown. Finally, if the final output turned symbol can't resolve to a class,
   then an error is thrown.

   Consider for instance:
   ```
   (shorthands '{ju java.util ju.c java.util.Concurrent ..java.util SOMETHING-REALLY-WRONG})
   #+! (nichnames {'ArrList ..ju.ArrayList})
   #+! (.getName ..j.u.c.ConcurrentHashMap) ; => \"java.util.concurrent.ConcurrentHashMap\" 
   #+! (.get (..ArrList/new [1 2 3]) 2) ; => 3 (emits same code as `(java.util.ArrayList. [1 2 3])`)
   #+! #(doto ^..j.u.List % (.add 4)) ; no reflection emitted (traverses `:tag` meta)
   #+! #+! #+! ..j.u.List ; => java.util.List (idempotent)
   ```

   (1) this is because of potentially unintuitive behaviors with how package qualifiers are detected,
   and due to the necessity to have a special case for nickname expansion. The choice was made to KISS."
  [frm]
  (let [ns-transform (ns->transform *ns*)
        transform-two-dots #(if-let [[_ proc] (re-matches #"[.][.](.+)" %)]
                              (ns-transform proc)
                              %)]
    (read-shortened* ::all transform-two-dots frm)))

(defn read-shortened-sym
  "The reader for the #-! reader-macro. 

   Similar behavior to the #+! reader-macro, 
   but only processes a single symbol (without even walking its meta) 
   and doesn't need the `..` prefix."
  [frm]
  (if (symbol? frm)
    (let [ns-transform (ns->transform *ns*)]
      (read-shortened* nil ns-transform frm))
    (throw (rex ClassCastException "Only symbols allowed" {:form frm}))))

(comment
  (do (shorthands '{j.u java.util j.u.c java.util.concurrent ..java.util SOMETHING-REALLY-WRONG})
      (nicknames {'ArrList java.util.ArrayList})
      (println (try (nicknames {'wonthappen (Class/forName "[Ljava.lang.Object;")})
                    (catch IllegalArgumentException iae iae)))
      (require '[clojure.pprint :as pprint])
      (binding [*print-meta* true *print-length* false]
        (println "+++TEST read-shortened+++")
        (pprint/pprint
         (mapv
          read-shortened
          '[(.getName ..j.u.c.ConcurrentHashMap)
            (.get (..ArrList/new '[..j.u.List 2 3]) 2)
            #(doto ^..j.u.List % (.add 4))
            ..j.u.List]))
        (println "+++TEST read-shortened-sym+++")
        (pprint/pprint (read-shortened-sym '^[..j.u.List j.u.List] j.u.List))
        (println (try (read-shortened-sym 'j.u.List.) (catch IllegalArgumentException iae iae)))
        (println (try (read-shortened-sym '..j.u.List) (catch IllegalArgumentException iae iae)))
        (println (try (read-shortened-sym 3) (catch ClassCastException cce cce))))))