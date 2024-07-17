# DOUBLEDOT

Nicknames and package shorthands that might actually work.

Born of half a week of having to deal with multiple Java classes with the same package but a different name, and of half an evening of spare time (the 80% of it, that is).

Never worry about that again! With only a bit of flagrant violation of Clojure's reserved symbols rules you too can experience the joy of `nicknames` for classes and `shorthands` for packages that work (almost) everywhere! If you aren't feeling adventurous or you just want your subforms to still make sense in isolation, there also is a reader macro to apply to single symbols that sadly makes the name of this library meaningless.

By default, the reader-macros `#+!` and `#-!` are bound respectively to `read-shortened` and `read-shortened-sym`,
but a `readerless` version is also available that lets you define your own instead.

Alpha.

# USAGE EXAMPLE

```clojure
(shorthands '{j.u java.util clj clojure.lang})

;; recursive reader (interacts safely with itself)
#+!
(defn foo
  [self]
  (condp instance? self
    ..clj.IPersistentMap :pmap
    #+! ..j.u.concurrent.ConcurrentHashMap :concurrent-hash-map
    ..j.u.Map :map))

;; single symbol reader with no `..` requirement (interacts safely with recursive reader)
;; also, note how `#+!` also works on metadata.
(def coll->array-list #+! ^[..j.u.Collection] #-! j.u.ArrayList/new)
```

# NREPL INTEGRATION
A very experimental and rudimentary `nrepl` integration is provided
by requiring `org.clojars.evenmoreirrelevance.doubledot.nrepl`.

# COORDINATES
- Version with reader-macros: https://clojars.org/org.clojars.evenmoreirrelevance/doubledot 
- Readerless version: https://clojars.org/org.clojars.evenmoreirrelevance/doubledot-readerless
