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
            :value "foo"}))))
