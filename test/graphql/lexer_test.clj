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
            :value "unicode \u1234\u5678\u90AB\uCDEF"}))))
