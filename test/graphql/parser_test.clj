(ns graphql.parser-test
  (:require [clojure.test :refer :all]
            [graphql.parser :refer [parse]]))

(deftest parser-test
  (testing "Parser"
    (testing "accepts option to not include source"
      (is (= {:kind :OperationDefinition
              :loc {:start 0 :end 9}
              :operation :query
              :name nil
              :variable-definitions nil
              :directives []
              :selection-set {:kind :SelectionSet
                              :loc {:start 0 :end 9}
                              :selections [{:kind :Field
                                            :loc {:start 2 :end 7}
                                            :alias nil
                                            :name {:kind :Name
                                                   :loc {:start 2 :end 7}
                                                   :value "field"}
                                            :arguments []
                                            :directives []
                                            :selection-set nil}]}}
             (parse "{ field }")))
      
      )))
