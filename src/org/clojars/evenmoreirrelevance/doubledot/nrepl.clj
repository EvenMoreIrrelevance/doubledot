(ns org.clojars.evenmoreirrelevance.doubledot.nrepl
  (:require
   [org.clojars.evenmoreirrelevance.doubledot :as doubledot]
   [org.clojars.evenmoreirrelevance.doubledot.detail :as detail]
   [clojure.string :as str]))

(doubledot/shorthands
 '{reflect java.lang.reflect
   util java.util
   clj clojure.lang})

(defn rfirst
  ([reducible]
   (rfirst identity reducible))
  ([xform reducible]
   (transduce
    xform (fn nothing-or-reduced-first
            ([] nil)
            ([r] r)
            ([_ nxt] (reduced nxt)))
    reducible)))

(do ;; hack around the inlined deps
  (def ^clojure.lang.Var clj-sources 
    (try (requiring-resolve 'cider.nrepl.middleware.complete/clj-sources) (catch Throwable _ nil)))
  (def ^:private compliment-version
    (when clj-sources
      (let [standalone-compliment-version?
            (rfirst (comp (map namespace)
                          (keep #(second (re-matches #"compliment(.*?)" %))))
                    @clj-sources)
            inlined-compliment-version
            (into #{} (comp (map namespace)
                            (keep #(second (re-matches #"cider[.]nrepl[.]inlined[.]deps[.]compliment[.](.+?)[.].*" %))))
                  @clj-sources)
            observed-versions
            (cond-> inlined-compliment-version
              standalone-compliment-version? (conj :not-inlined))]
        #_"if there are conflicts here, bail out..."
        (and (= 1 (count observed-versions))
             (first observed-versions)))))
  (def ^:private sources-ns-name
    (when compliment-version
      (case compliment-version
        :not-inlined "compliment.sources"
        (str "cider.nrepl.inlined.deps.compliment." compliment-version ".compliment.sources"))))
  (def ^:private compliment-sources-var
    (if compliment-version
      (fn compliment-sources-var
        ([name_] (compliment-sources-var "" name_))
        ([extra-ns name_]
         (requiring-resolve (symbol (str sources-ns-name extra-ns) name_))))
      (constantly nil)))

  (def ^:private compliment-class-candidates (compliment-sources-var ".classes" "candidates"))
  (def ^:private compliment-static-member-candidates (compliment-sources-var ".class-members" "static-members-candidates"))
  (def ^:private compliment-get-all-members (compliment-sources-var ".class-members" "get-all-members"))
  (def ^:private compliment-defsource (compliment-sources-var "defsource"))
  (def have-completions?
    (and compliment-class-candidates
         compliment-get-all-members
         compliment-static-member-candidates
         compliment-defsource
         true)))

(def shorthand-candidates
  (if-not have-completions?
    (constantly [])
    (fn [prefix ns_ opts]
      (when (str/starts-with? prefix "..")
        (let [shorthands-map (detail/raw-ns-shorthands ns_)]
          (or (when-let [parsed (detail/parse-shortened (subs prefix 2) shorthands-map)]
                (map (fn [{:as candidate}]
                       (update candidate :candidate #(str prefix (subs % (count parsed)))))
                     (compliment-class-candidates parsed ns_ opts)))
              (let [pointless (cond-> prefix (str/ends-with? prefix ".") (subs 0 (dec (count prefix))))]
                (sequence (comp (filter (fn [[k _]] (str/starts-with? k (subs pointless 2))))
                                (map (fn [[k v]]
                                       (map (fn [{:as candidate}]
                                              (update candidate :candidate #(str ".." k (subs % (count v)))))
                                            (compliment-class-candidates v ns_ opts))))
                                cat)
                          shorthands-map))))))))

 (def shorthand-member-candidates
   (if-not have-completions?
     (constantly [])
     (fn [prefix ns_ opts]
       (when (str/starts-with? prefix "..")
         (let [shorthands-map (detail/raw-ns-shorthands ns_)]
           (when-let [[_ sym-ns sym-name] (re-matches #"(.+?)/(.*)" prefix)]
             (when-let [^Class the-class (try (Class/forName
                                               (detail/parse-shortened
                                                (subs sym-ns 2) shorthands-map))
                                              (catch ClassNotFoundException _ nil))]
               (concat
                (map (fn fix-candidate [c]
                       (update c :candidate
                               #(str sym-ns (subs % (count (.getName the-class))))))
                     (compliment-static-member-candidates
                      (str (.getName the-class) "/" sym-name)
                      ns_ opts))
                (sequence (comp (filter (fn starts-well [[mn _]] (str/starts-with? mn sym-name)))
                                (keep (fn some-member-accessible [[_ ^java.lang.reflect.Member ms]]
                                        (rfirst
                                         (filter #(.isAssignableFrom (.getDeclaringClass ^java.lang.reflect.Member %) the-class))
                                         ms)))
                                (map (fn mem->candidate [^java.lang.reflect.Member m]
                                       {:candidate (str sym-ns "/" (.getName m))
                                        :type (condp instance? m
                                                java.lang.reflect.Field :field
                                                java.lang.reflect.Method :method)})))
                          (compliment-get-all-members 'clojure.core the-class))))))))))

(defn constantly-empty-str [& _args] "")

#_{:clj-kondo/ignore [:unused-binding]}
 (defn ^:private real-defsource
   [name & {:keys [:candidates :doc] :as source}]
   (let [out (compliment-defsource name source)]
     (alter-var-root clj-sources
                     #(cond-> %
                        (= -1 (.indexOf ^java.util.List % name)) (conj name)))
     out))

(do (when have-completions?
      (real-defsource
       ::shorthands :candidates #'shorthand-candidates :doc #'constantly-empty-str))
    (when have-completions?
      (real-defsource
       ::shorthands-members :candidates #'shorthand-member-candidates :doc #'constantly-empty-str)))

(comment
  (require '[nrepl.core :as nrepl])
  (defn ask-nrepl
    [& {:as stuff}]
    (with-open [conn (nrepl/connect :port (parse-long (slurp ".nrepl-port")))]
      (vec (nrepl/message
            (nrepl/client conn 1000000)
            stuff))))

  (ask-nrepl {:op "ls-middleware"})
  (def nrepl-ops (first (ask-nrepl {:op "describe" :verbose? "true"})))
  (ask-nrepl {:op "complete"
              :symbol "..util.List/"
              :ns (str (.getName *ns*))}))
