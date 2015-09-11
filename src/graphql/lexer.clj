(ns graphql.lexer)

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

(defn- pos-started [body]
  (reduce (fn [acc i]
            (if (some #{i} '(\space \newline))
              (inc acc)
              (reduced acc)))
          0 (seq (char-array body))))

(defn- readName [body, position]
  (let [end (reduce (fn [acc i]
                      (if (or (= \_ i)
                              (and (>= (int i) 48) (<= (int i) 57))
                              (and (>= (int i) 65) (<= (int i) 90))
                              (and (>= (int i) 97) (<= (int i) 122)))
                        (inc acc)
                        (reduced acc)))
                    position (seq (char-array (subs body position))))]
    {:kind (:NAME token-kind)
     :start position
     :end end
     :value (subs body position end)}))

(defn read [ql]
  (let [position (pos-started ql)]
    (readName ql position)))
