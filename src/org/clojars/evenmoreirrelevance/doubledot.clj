(ns org.clojars.evenmoreirrelevance.doubledot
  (:require 
   #_[clojure.string :as str]
   [clojure.core.protocols]
   [org.clojars.evenmoreirrelevance.doubledot.detail :as detail]))

#_(defn nicknames
    "Sets nicknames for the given classes.
   The classes must belong to a package and must `pr` to a simple-symbol."
    ([{:as nicks->classes}]
     (nicknames *ns* nicks->classes))
    ([ns_ {:as nicks->classes}]
     (detail/effects!
      (let [ns-name (.getName ^clojure.lang.Namespace ns_)]
        (swap! detail/*ns-> update-in [::detail/nickname->classname ns-name]
               (fn [ns-nicknames->classnames]
                 (-> (or ns-nicknames->classnames {})
                     (detail/merge-self-with*
                      (fn [k o n]
                        (if (or (nil? o) (= o n))
                          n
                          (throw (detail/rex IllegalStateException
                                             "Nickname already in use"
                                             {:nickname k :old-target o :new-target n}))))
                      (-> (detail/conform!! ::detail/nicks->classes nicks->classes)
                          (update-keys name)
                          (update-vals #(cond
                                          (not (str/includes? (.getName ^Class %) "."))
                                          (throw (detail/rex IllegalArgumentException
                                                             "Classes which aren't package-qualified are disallowed."
                                                             {:input %}))
                                          (not (detail/class-edn-read-roundtrips? %))
                                          (throw (detail/rex IllegalArgumentException
                                                             "Classname doesn't read as a simple-symbol."
                                                             {:input %}))
                                          :else (.getName ^Class %))))))))))))

#_(defn unnickname
    ([[:as more]] (unnickname *ns* more))
    ([ns_ [:as more]]
     (detail/effects!
      (swap! detail/*ns->
             update-in [::detail/nickname->classname (.getName ^clojure.lang.Namespace ns_)]
             #(reduce dissoc % %2) (map #(name (cast clojure.lang.Symbol %)) more)))))

#_(defn ns-nicknames
    "Gets the nicknames in effect in `ns_`.
   Note that you will get a fresh map every time you call this."
    [ns_]
    (some->> (detail/raw-ns-nicknames ns_) (into {} (map #(mapv symbol %)))))

(defn shorthands
  "Defines shorthands in `ns_` (default *ns*). 
   Fails atomically if it would override a shorthand."
  ([{:as shorthand->package}] (shorthands *ns* shorthand->package))
  ([ns_ {:as shorthand->package}]
   (detail/effects!
    (let [ns-sym (.getName ^clojure.lang.Namespace ns_)]
      (swap! detail/*ns-> update-in [::detail/shorthand->package ns-sym]
             (fn [ns-shorthand->package]
               (-> (or ns-shorthand->package (sorted-map-by detail/compare-by-prefix-length-alpha))
                   (detail/merge-self-with* (fn [k o n]
                                              (if (= o n)
                                                n
                                                (throw (detail/rex IllegalStateException
                                                                   "Shorthand already in use"
                                                                   {:shorthand k
                                                                    :old-target o
                                                                    :new-target n}))))
                                            (-> (detail/conform!! ::detail/pkg->alias shorthand->package)
                                                (update-vals name)
                                                (update-keys name))))))))))

(defn unshorthand
  "Removes shorthands in `ns` (default *ns*). Will ignore nonexisting shorthands."
  ([more] (unshorthand *ns* more))
  ([ns_ [& more]]
   (detail/effects!
    (swap! detail/*ns-> update-in [::detail/shorthand->package (.getName ^clojure.lang.Namespace ns_)]
           #(reduce dissoc % %2) (map #(name (cast clojure.lang.Symbol %)) more)))))

(defn ns-shorthands
  "Gets the shorthands in effect for the `ns_`. 
   Note that you get a fresh map every time you call this."
  [ns_]
  (some->> (detail/raw-ns-shorthands ns_) (into {} (map #(mapv symbol %)))))

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

   Processing works as follows: if the ns/name ends with `.`, then an error is thrown(1).
   Then, the `..` prefix is elided, and the longest shorthand such that adding a dot 
   at the end of it yields a proper prefix of the ns/name is replaced by its long hand form.
   If no shorthand is found, or the final output doesn't resolve to a class,
   then an error is thrown.

   Consider for instance:
   ```
   (shorthands '{ju java.util ju.c java.util.Concurrent ..java.util SOMETHING-REALLY-WRONG})
   #+! (.getName ..j.u.c.ConcurrentHashMap) ; => \"java.util.concurrent.ConcurrentHashMap\" 
   #+! (.get (..j.u.List/new [1 2 3]) 2) ; => 3 (emits same code as `(java.util.ArrayList. [1 2 3])`)
   #+! #(doto ^..j.u.List % (.add 4)) ; no reflection emitted (traverses `:tag` meta)
   #+! #+! #+! ..j.u.List ; => java.util.List (idempotent)
   ```

   (1) this is because of potentially unintuitive behaviors with how package qualifiers are detected,
   and due to the necessity to have a special case for nickname expansion. The choice was made to KISS."
  [frm]
  (let [ns-transform (detail/ns->transform *ns*)
        transform-two-dots #(if-let [[_ proc] (re-matches #"[.][.](.+)" %)]
                              (ns-transform proc)
                              %)]
    (detail/read-shortened* ::detail/all transform-two-dots frm)))

(defn read-shortened-sym
  "The reader for the #-! reader-macro. 

   Similar behavior to the #+! reader-macro, 
   but only processes a single symbol (without even walking its meta) 
   and doesn't need the `..` prefix."
  [frm]
  (if (symbol? frm)
    (let [ns-transform (detail/ns->transform *ns*)]
      (detail/read-shortened* nil ns-transform frm))
    (throw (detail/rex ClassCastException "Only symbols allowed" {:form frm}))))

(comment
  (do (shorthands '{j.u java.util j.u.c java.util.concurrent ..java.util SOMETHING-REALLY-WRONG})
      #_(nicknames {'ArrList java.util.ArrayList})
      #_(println (try (nicknames {'wonthappen (Class/forName "[Ljava.lang.Object;")})
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
        (pprint/pprint (read-shortened-sym '^..j.u.List j.u.List))
        (println (try (read-shortened-sym 'j.u.List.) (catch IllegalArgumentException iae iae)))
        (println (try (read-shortened-sym '..j.u.List) (catch IllegalArgumentException iae iae)))
        (println (try (read-shortened-sym 3) (catch ClassCastException cce cce))))))