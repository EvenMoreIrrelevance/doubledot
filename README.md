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
(nichnames {'DomNode org.w3c.dom.Node})

;; recursive reader (interacts safely with itself)
#+!
(defn foo
  [self]
  (condp instance? self
    ..clj.IPersistentMap :pmap
    #+! ..j.u.concurrent.ConcurrentHashMap :concurrent-hash-map
    ..j.u.Map :map
    ..DomNode :dom-node!?))

;; single symbol reader with no `..` requirement (interacts safely with recursive reader)
;; also, note how `#+!` also works on metadata.
(def coll->array-list #+! ^[..j.u.Collection] #-! j.u.ArrayList/new)
```

# COORDINATES

```
emi/doubledot {:git/url "https://github.com/EvenMoreIrrelevance/doubledot"
               :git/sha "e6a71aabe2bc50872db8c300a4470bb74bfe9d29"}

emi/doubledot-readerless {:git/url "https://github.com/EvenMoreIrrelevance/doubledot"
                          :git/sha "b8594f911ce68aa64d13c8a3157d33a0a532e847"}
```
