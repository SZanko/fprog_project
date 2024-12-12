(ns fprog-project.core-test
  (:require [clojure.test :refer :all]
            [fprog-project.core :refer :all]
            [clojure.java.io :as io]))



(def example-tree-no-left
  (->TreeNode :black nil "Value" (->TreeNode :red nil "Value2" nil)))

(deftest branch-test
  (testing "branch? function"
    (is (= nil (branch? nil)))
    (is (= nil (branch? example-tree-no-children)))
    (is (= (:left example-tree) (branch? example-tree)))
    (is (= (:right example-tree-no-left (branch? example-tree-no-left))))))

(deftest children-test
  (testing "children function"
    (is (= () (children nil)))
    (is (= () (children example-tree-no-children)))
    (is (= (list (:right example-tree-no-left)) (children example-tree-no-left)))
    (is (= (list (:left example-tree) (:right example-tree)) (children example-tree)))))

(deftest get-tree-content-test
  (testing "get-tree-content-function"
    (is (= '(nil) (get-tree-content nil)))
    (is (= '(:value example-tree-no-children) (get-tree-content example-tree-no-children)))
    (is (= '(:value example-tree-no-left (:value (:right example-tree-no-left))) (get-tree-content example-tree-no-left)))
    (is (= '(:value example-tree-no-left (:value (:right example-tree)) (:value (:left example-tree))) (get-tree-content example-tree)))))


(deftest find-val-test
  (testing "find-val function"
    (is (= (find-val-node empty-tree "Tmp") nil))
    (is (= (find-val-node example-tree "Informatik") (->TreeNode :red nil "Informatik" nil)))))


(deftest get-tree-content-test
  (testing "get-tree-content function"
    (is (= '(nil) (get-tree-content empty-tree)))
    (is (= '("Data Science" "Funktionale Programmierung" "Informatik") (get-tree-content example-tree)))))

(deftest balance-test
  (testing "tries to balance a tree"
    (is (= nil (balance empty-tree))))
  (is (= example-tree-no-children (balance example-tree-no-children)))
  ;(is (= example-tree-left-unbalanced (balance example-tree-left-unbalanced)))
  (is (= example-tree-no-left (balance example-tree-no-left)))
  (is (= example-tree (balance example-tree)))
  )

(deftest insert-val-test
  (testing "inserts into tree")
  (is (= example-tree-no-children (insert-val empty-tree (->TreeNode nil nil "Value" nil))))
  ; broken
  (let [tmp (->TreeNode nil nil "addedValue" nil)
        tmp-after-insert (assoc tmp :color red)]
    (is (= (assoc example-tree-no-children :right tmp-after-insert) (insert-val example-tree-no-children tmp)))
  )
  ; balance error
  (is (= example-tree (insert-val example-tree (->TreeNode nil nil "newValue" nil))))
  )


; broken
(deftest write-tree-to-file-test
  (testing "write-tree-to-file function"
    (is (= (.exists (io/file "resources/testing.txt")) false))
    (is (= (write-tree-to-file example-tree "resources/testing.txt") nil))
    (is (= (slurp "resources/testing.txt") (get-tree-content example-tree)))
    (is (= (.exists (io/file "resources/testing.txt")) true)))

  (io/delete-file "resources/testing.txt" true))
