(ns fprog-project.core-test
  (:require [clojure.test :refer :all]
            [fprog-project.core :refer :all]
            [clojure.java.io :as io]))


(deftest a-test
  (testing "FIXME, I fail."
    (is (= 1 1))))

(deftest find-val-test
  (testing "find-val function"
    (is (= (find-val-node empty-tree "Tmp") nil)))
  (is (= (find-val-node example-tree "Informatik") (->TreeNode :red nil "Informatik" nil))))


(deftest get-tree-content-test
  (testing "get-tree-content function"
    (is (= '(nil) (get-tree-content empty-tree)))
    (is (= '("Data Science" "Funktionale Programmierung" "Informatik") (get-tree-content example-tree)))))


; broken
(deftest write-tree-to-file-test
  (testing "write-tree-to-file function"
    (is (= (.exists (io/file "resources/testing.txt")) false))
    (is (= (write-tree-to-file example-tree "resources/testing.txt") nil))
    (is (= (slurp "resources/testing.txt") (get-tree-content example-tree)))
    (is (= (.exists (io/file "resources/testing.txt")) true)))

  (io/delete-file "resources/testing.txt" true))
