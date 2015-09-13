(ns graphql.parser
  (:require [graphql.lexer :refer [read token-kind]]))

(defn- make-parser [source]
  (atom {:token (read source)
         :source source
         :prev-end 0}))

(defn- advance [parser]
  (swap! parser
         (fn [prev]
           (let [prev-end (get-in prev [:token :end])]
             (-> prev
                 (assoc :prev-end prev-end)
                 (assoc :token (read (:source prev) prev-end)))))))

;; (advance (make-parser "{hehe}"))

(defn- peek [parser kind]
  (= kind (get-in @parser [:token :kind])))

(defn- loc [parser start]
  {:start start
   :end (:prev-end @parser)})

(defn- many [parser open f close]
  (loop [node []
         cur (advance parser)]
    (if (= close (get-in cur [:token :kind]))
      (do (advance parser) node)
      (recur (conj node (f parser)) @parser))))

(defn- parse-name [parser]
  (let [{token :token} @parser
        next (advance parser)]
    {:kind :Name
     :value (token :value)
     :loc (loc parser (token :start))}))

(defn- parse-field [parser]
  (let [start (get-in @parser [:token :start])
        name (parse-name parser)]
    {:kind :Field
     :alias nil
     :name name
     :arguments []
     :directives []
     :selection-set nil
     :loc (loc parser start)}))


(defn- parse-selection [parser]
  (parse-field parser))

(defn- parse-selection-set [parser]
  (let [start (get-in  @parser [:token :start])
        selections (many parser (token-kind :BRACE_L) parse-selection (token-kind :BRACE_R))]
    {:kind :SelectionSet
     :selections selections
     :loc (loc parser start)}))


(defn- parse-op-def [parser]
  (let [start (get-in @parser [:token :start])
        brace (peek parser (:BRACE_L token-kind))
        selection-set (parse-selection-set parser)]
    (if brace
      {:kind :OperationDefinition
       :operation :query
       :name nil
       :variable-definitions nil
       :directives []
       :selection-set selection-set
       :loc (loc parser start)
       })))
(defn parse [source]
  (parse-op-def (make-parser source)))

