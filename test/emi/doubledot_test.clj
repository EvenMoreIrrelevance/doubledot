(ns emi.doubledot-test
  (:require [clojure.test :as test]
            [emi.doubledot :as dd]))

(defmacro ^:private clearing-shorthands
  [& body]
  `(try (do ~@body)
        (finally (dd/unnickname (keys (dd/ns-nicknames *ns*)))
                 (dd/unshorthand (keys (dd/ns-shorthands *ns*))))))

(test/deftest nicknames
  (test/testing "happy path"
    (clearing-shorthands
     (do (test/is (any? (dd/nicknames {'Objects Class})))
         (test/is (= '{Objects java.lang.Class} (dd/ns-nicknames *ns*))))
     (do (test/is (any? (dd/unnickname '[Objects])))
         (test/is (= {} (dd/ns-nicknames *ns*))))))

  (test/testing "blank nicknames disallowed"
    (clearing-shorthands
     (test/is (thrown? IllegalArgumentException (dd/nicknames {(symbol "  ") Class})))))

  (test/testing "classes whose names read into simple-symbols disallowed"
    (clearing-shorthands
     (test/is (thrown? IllegalArgumentException (dd/nicknames {'nick (class (object-array 1))}))))
    (clearing-shorthands
     (do (eval (list `deftype (symbol "Invalid@Type") []))
         (let [evil-class (Class/forName "emi.doubledot_test.Invalid@Type")]
           (test/is (thrown? IllegalArgumentException (dd/nicknames {'nick evil-class})))))))

  (test/testing "classes with no package qualification disallowed"
    (clearing-shorthands
     (test/is (thrown? IllegalArgumentException (dd/nicknames {'nick Integer/TYPE})))))

  (test/testing "no nickname redefinitions without unnicknaming first"
    (clearing-shorthands
     (dd/nicknames {'nick Class})
     (test/is (any? (dd/nicknames {'nick Class})))
     (test/is (thrown? IllegalStateException (dd/nicknames {'nick Object}))))))

(test/deftest shorthands
  (test/testing "happy path"
    (clearing-shorthands
     (test/is (any? (dd/shorthands '{j.u java.util})))
     (test/is (= '{j.u java.util} (dd/ns-shorthands *ns*)))
     (test/is (any? (dd/unshorthand '[j.u])))
     (test/is (= {} (dd/ns-shorthands *ns*)))))

  (test/testing "no blanks anywhere"
    (clearing-shorthands
     (test/is (thrown? IllegalArgumentException (dd/shorthands {'short (symbol " ")})))
     (test/is (thrown? IllegalArgumentException (dd/shorthands {(symbol "\n\t") 'lh})))))

  (test/testing "no shorthand redefinition without unshorthanding first"
    (clearing-shorthands
     (dd/shorthands '{short lh})
     (test/is (any? (dd/shorthands '{short lh})))
     (test/is (thrown? IllegalStateException (dd/shorthands '{short lh1}))))))

(test/deftest read-shortened
  (clearing-shorthands
   (dd/shorthands '{j.u java.util u util j java})
   (dd/nicknames {'DomNode org.w3c.dom.Node})
   (test/testing "simple"
     (test/are [lh sh] (= lh (dd/read-shortened sh))
       'java.util.Map '..j.u.Map
       'java.util.concurrent.ConcurrentHashMap '..j.u.concurrent.ConcurrentHashMap
       'org.w3c.dom.Node '..DomNode
       'java.util.Map/add '..j.u.Map/add)) 

   (test/testing "walks forms"
     (test/is (= '[java.util.Map org.w3c.dom.Node] (dd/read-shortened '[..j.u.Map ..DomNode])))
     (test/is (= '{java.util.Map 2 3 org.w3c.dom.Node} (dd/read-shortened '{..j.u.Map 2 3 ..DomNode}))))
   (test/testing "spares quoted forms"
     (test/is (= '(quote ..j.u.Map) (dd/read-shortened '(quote ..j.u.Map)))))
   (test/testing "spares literals"
     (test/is (= '..j.u.Map (dd/read-shortened '^::dd/l ..j.u.Map))))
   (test/testing "walks meta"
     (test/is (= 'java.util.Map (-> (dd/read-shortened '^..j.u.Map []) meta :tag)))
     (test/is (= 'java.util.Map (-> (dd/read-shortened '^..j.u.Map (quote 3)) meta :tag)))
     (test/is (= 'java.util.Map (-> (dd/read-shortened '^^..j.u.Map a b) meta :tag meta :tag))))

   (test/testing "refuses working with symbols ending with `.`"
     (test/is (thrown? IllegalArgumentException (dd/read-shortened '..j.u.Map.))))
   (test/testing "only works if the output resolves to a class"
     (test/is (thrown? IllegalArgumentException (dd/read-shortened '..j.u.NotSomethingInJavaUtils!!!))))
   (test/testing "if the symbol has a ns, the name isn't processed"
     (test/is (= 'java.util.Map/..j.u.Map (dd/read-shortened '..j.u.Map/..j.u.Map)))
     (test/is (= 'NotAClassForSure/..j.u.Map (dd/read-shortened 'NotAClassForSure/..j.u.Map))))
   (test/testing "Shorthands only work when they're prefixes"
     (test/is (thrown? IllegalArgumentException (dd/read-shortened '..java.u.Map))))))

(test/deftest read-shortened-sym
  (clearing-shorthands
   (dd/shorthands '{j.u java.util u util})
   (dd/nicknames {'DomNode org.w3c.dom.Node})
   (test/testing "only accepts symbols"
     (test/is (thrown? ClassCastException (dd/read-shortened-sym []))))
   (test/testing "doesn't require .. before symbol"
     (test/is (= 'java.util.Map (dd/read-shortened-sym 'j.u.Map)))
     (test/is (= 'org.w3c.dom.Node (dd/read-shortened-sym 'DomNode))))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(deftype List [])

(test/deftest reader-interactions
  (test/testing "#+! #+!"
    (clearing-shorthands
     (dd/shorthands '{j.u ..java.util java.util emi.doubledot_test})
     ;; note that the expected behavior is for this to throw 
     ;; because `..java.util.List `never resolve to class.
     (test/is (not= 'emi.doubledot_test.List
                    (try (dd/read-shortened (dd/read-shortened 'j.u.List))
                         (catch Throwable _ nil))))))
  (test/testing "#-! #-!"
    (clearing-shorthands
     (dd/shorthands '{j.u java.util java.util emi.doubledot_test})
     (test/is (= (dd/read-shortened-sym 'j.u.List)
                 (-> 'j.u.List
                     (dd/read-shortened-sym)
                     (dd/read-shortened-sym))))))
  (test/testing "#-! #+!" ;; I *might* just make this blow up lmao.
    (clearing-shorthands
     (dd/shorthands '{j.u java.util java.util emi.doubledot_test})
     (test/is (= (dd/read-shortened '..j.u.List)
                 (-> '..j.u.List (dd/read-shortened) (dd/read-shortened-sym))))))
  (test/testing "#+! #-!"
    (clearing-shorthands
     (dd/shorthands '{j.u ..java.util java.util emi.doubledot_test})
     (test/is (not= 'emi.doubledot_test.List
                    (try (-> 'j.u.List (dd/read-shortened-sym) (dd/read-shortened))
                         (catch Throwable _ nil)))))))

(test/run-tests)