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

(def mapping-kind
  {\! (token-kind :BANG)
   \$ (token-kind :DOLLAR)
   \( (token-kind :PAREN_L)
   \) (token-kind :PAREN_R)
   \: (token-kind :COLON)
   \= (token-kind :EQUALS)
   \@ (token-kind :AT)
   \[ (token-kind :BRACKET_L)
   \] (token-kind :BRACKET_R)
   \{ (token-kind :BRACE_L)
   \} (token-kind :BRACE_R)
   \| (token-kind :PIPE)})

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

(defn- read-number [ql pos]
  (let [body (subs ql pos)
        matched-int (re-find #"^-?(0|[1-9][0-9]*)" body)
        matched-float (re-find #"^-?(0|[1-9][0-9]*)(\.[0-9]+)?((E|e)(\+|\-)?[0-9]+)?" body)
        count-int (count (first matched-int))
        count-float (count (first matched-float))]
    (if (= count-int count-float)
      [(+ pos count-int) (token-kind :INT)]
      [(+ pos count-float) (token-kind :FLOAT)])))

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
      (or (= \- current) (and (>= (int current) 48) (<= (int current) 57)))
      (let [[end type] (read-number ql start)]
                       {:kind type
                        :start start
                        :end end
                        :value (subs ql start end)})
      (some #{current} '(\! \$ \( \) \: \= \@ \[ \] \{ \} \|))
      {:kind (mapping-kind current)
       :start start
       :end (inc start)
       :value nil}
      (= \. current) (if (and (= \. (qlchars (+ 2 pos))) (= \. (qlchars (inc pos))))
                       {:kind (token-kind :SPREAD)
                        :start start
                        :end (+ 3 start)
                        :value nil}
                       :error)
      :else (let [end (end-of ql start read-name)]
              {:kind (:NAME token-kind)
               :start start
               :end end
               :value (subs ql start end)})
      )))

(defn read [ql]
  (make-token ql 0))
