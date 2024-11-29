(ns fprog-project.core-test
  (:require [clojure.test :refer :all]
            [fprog-project.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 1 1))))

(deftest find-val-test
  (testing "find-val function"
    (is (= (find-val-node empty-tree "Tmp") nil)))
    (is (= (find-val-node example-tree "Informatik") (->TreeNode :red nil "Informatik" nil) ))
  )
