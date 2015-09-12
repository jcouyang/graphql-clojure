(ns graphql.lexer
  (:require [clojure.core.match :refer [match]]
            [reduce-fsm :as fsm]))

(def token-kind
  {:EOF 1
   :BANG 2
   :DOLLAR 3
   :PAREN_L 4
   :PAREN_R 5
   :SPREAD 6
   :COLON 7
   :EQUALS 8
   :AT 9
   :BRACKET_L 10
   :BRACKET_R 11
   :BRACE_L 12
   :PIPE 13
   :BRACE_R 14
   :NAME 15
   :VARIABLE 16
   :INT 17
   :FLOAT 18
   :STRING 19})

(defn- end-of [body pos f]
  (reduce f pos (seq (char-array (subs body pos)))))

(defn- comments [acc i]
  (cond
    (some #{i} '(\newline \return)) (reduced acc)
    :else (inc acc)))
  
(defn- end-of-whitespace [body pos]
  (let [vbody (vec (char-array body))
        length (count vbody)]
    (loop [current pos]
      (cond
        (>= current length) current
        (= \# (vbody current)) (recur (inc (end-of body current comments)))
        (some #{(vbody current)} '(\space \newline \,)) (recur (inc current))
        (and (> (int (vbody current)) 8) (< (int (vbody current)) 14)) (recur (inc current))
        :else current))))

(end-of-whitespace "#comment
                      foo#comment" 0)


(defn- read-name [acc i]
  (if (or (= \_ i)
          (and (>= (int i) 48) (<= (int i) 57))
          (and (>= (int i) 65) (<= (int i) 90))
          (and (>= (int i) 97) (<= (int i) 122)))
    (inc acc)
    (reduced acc)))

(defn inc-pos [acc & _ ] (inc acc))
(fsm/defsm make-string
  [[:start
    \" -> {:action inc-pos} :into-string]
   [:into-string
    \" -> {:action inc-pos} :end
    \\ -> {:action inc-pos} :ignore
    _ -> {:action inc-pos} :into-string]
   [:ignore
    _ -> {:action inc-pos} :into-string]
   [:end
    _ -> :end]])

(defn make-token [ql pos]
  (let [qlchars (vec (char-array ql))
        start (end-of-whitespace ql pos)
        current (qlchars start)]
    (cond
      (= \" current) (let [end (make-string start ql)]
                       {:kind (token-kind :STRING)
                        :start start
                        :end end
                        :value (read-string (subs ql start end))})
      :else (let [end (end-of ql start read-name)]
              {:kind (:NAME token-kind)
               :start start
               :end end
               :value (subs ql start end)})
      )))

(defn read [ql]
  (make-token ql 0))
