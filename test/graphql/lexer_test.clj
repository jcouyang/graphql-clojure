(ns graphql.lexer-test
  (:require [clojure.test :refer :all]
            [graphql.lexer :as lexer]))

(testing "Lexer"
  (testing "skips wihtespace"
    (is (= (lexer/read "
    foo
")
           {:kind 15
            :start 5
            :end 8
            :value "foo"}))
    (is (= (lexer/read "#comment
                      foo#comment")
           {:kind 15
            :start 31
            :end 34
            :value "foo"}))
    (is (= (lexer/read ",,,foo,,,")
           {:kind 15
            :start 3
            :end 6
            :value "foo"})))
  (testing "string"
    (is (= (lexer/read "\"simple\"")
           {:kind (lexer/token-kind :STRING)
            :start 0
            :end 8
            :value "simple"}))
    (is (= (lexer/read "\" white space \"")
           {:kind (lexer/token-kind :STRING)
            :start 0
            :end 15
            :value " white space "}))
    (is (= (lexer/read "\"quote \\\"\"")
           {:kind (lexer/token-kind :STRING)
            :start 0
            :end 10
            :value "quote \""}))
    (is (= (lexer/read "\"unicode \\u1234\\u5678\\u90AB\\uCDEF\"")
           {:kind (lexer/token-kind :STRING)
            :start 0
            :end 34
            :value "unicode \u1234\u5678\u90AB\uCDEF"})))
  (testing "number"
    (is (= (lexer/read "4")
           {:kind (lexer/token-kind :INT)
            :start 0
            :end 1
            :value "4"}))
    (is (= (lexer/read "4.123")
           {:kind (lexer/token-kind :FLOAT)
            :start 0
            :end 5
            :value "4.123"}))
    (is (= (lexer/read "-4")
           {:kind (lexer/token-kind :INT)
            :start 0
            :end 2
            :value "-4"}))
    (is (= (lexer/read "9")
           {:kind (lexer/token-kind :INT)
            :start 0
            :end 1
            :value "9"}))
    (is (= (lexer/read "0")
           {:kind (lexer/token-kind :INT)
            :start 0
            :end 1
            :value "0"}))
    (is (= (lexer/read "-4.123")
           {:kind (lexer/token-kind :FLOAT)
            :start 0
            :end 6
            :value "-4.123"}))
    (is (= (lexer/read "0.123")
           {:kind (lexer/token-kind :FLOAT)
            :start 0
            :end 5
            :value "0.123"}))
    (is (= (lexer/read "123E4")
           {:kind (lexer/token-kind :FLOAT)
            :start 0
            :end 5
            :value "123E4"}))
    (is (= (lexer/read "123e-4")
           {:kind (lexer/token-kind :FLOAT)
            :start 0
            :end 6
            :value "123e-4"}))
    (is (= (lexer/read "123e+4")
           {:kind (lexer/token-kind :FLOAT)
            :start 0
            :end 6
            :value "123e+4"}))
    (is (= (lexer/read "-1.123e4")
           {:kind (lexer/token-kind :FLOAT)
            :start 0
            :end 8
            :value "-1.123e4"}))
    (is (= (lexer/read "-1.123e4567")
           {:kind (lexer/token-kind :FLOAT)
            :start 0
            :end 11
            :value "-1.123e4567"})))
  (testing "punctuation"
    (is (= (lexer/read "!")
           {:kind (lexer/token-kind :BANG)
            :start 0
            :end 1
            :value nil}))
    (is (= (lexer/read "$")
           {:kind (lexer/token-kind :DOLLAR)
            :start 0
            :end 1
            :value nil}))
    (is (= (lexer/read "(")
           {:kind (lexer/token-kind :PAREN_L)
            :start 0
            :end 1
            :value nil}))
    (is (= (lexer/read ")")
           {:kind (lexer/token-kind :PAREN_R)
            :start 0
            :end 1
            :value nil}))
    (is (= (lexer/read "...")
           {:kind (lexer/token-kind :SPREAD)
            :start 0
            :end 3
            :value nil}))
    (is (= (lexer/read ":")
           {:kind (lexer/token-kind :COLON)
            :start 0
            :end 1
            :value nil}))
    (is (= (lexer/read "=")
           {:kind (lexer/token-kind :EQUALS)
            :start 0
            :end 1
            :value nil}))
    (is (= (lexer/read "@")
           {:kind (lexer/token-kind :AT)
            :start 0
            :end 1
            :value nil}))
    (is (= (lexer/read "[")
           {:kind (lexer/token-kind :BRACKET_L)
            :start 0
            :end 1
            :value nil}))
    (is (= (lexer/read "]")
           {:kind (lexer/token-kind :BRACKET_R)
            :start 0
            :end 1
            :value nil}))
    (is (= (lexer/read "{")
           {:kind (lexer/token-kind :BRACE_L)
            :start 0
            :end 1
            :value nil}))
    (is (= (lexer/read "}")
           {:kind (lexer/token-kind :BRACE_R)
            :start 0
            :end 1
            :value nil}))
    (is (= (lexer/read "|")
           {:kind (lexer/token-kind :PIPE)
            :start 0
            :end 1
            :value nil}))
    ))
