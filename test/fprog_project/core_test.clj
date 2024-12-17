(ns fprog-project.core-test
  (:require [clojure.test :refer :all]
            [fprog-project.core :refer :all]))


(deftest test-branch?
  (testing "branch? function"
    (let [leaf-node (->TreeNode :black nil "leaf" nil)
          branch-node (->TreeNode :black
                                  (->TreeNode :red nil "left" nil)
                                  "branch"
                                  (->TreeNode :red nil "right" nil))
          nil-node nil]
      (is (not (branch? leaf-node)) "Leaf node should return false")
      (is (branch? branch-node) "Node with children should return true")
      (is (not (branch? nil-node)) "Nil node should return false"))))


(deftest negative-test-branch?
  (testing "branch? should correctly identify nodes with children"
    (let [leaf-node (->TreeNode :black nil "leaf" nil)
          left-branch-node (->TreeNode :black (->TreeNode :red nil "left" nil) "node" nil)
          right-branch-node (->TreeNode :black nil "node" (->TreeNode :red nil "right" nil))
          full-branch-node (->TreeNode :black
                                       (->TreeNode :red nil "left" nil)
                                       "node"
                                       (->TreeNode :red nil "right" nil))]
      (is (not (branch? leaf-node)) "Leaf node without children should not be a branch")
      (is (branch? left-branch-node) "Node with left child should be a branch")
      (is (branch? right-branch-node) "Node with right child should be a branch")
      (is (branch? full-branch-node) "Node with both children should be a branch"))))

(deftest test-children
  (testing "children function"
    (let [leaf-node (->TreeNode :black nil "leaf" nil)
          branch-node (->TreeNode :black
                                  (->TreeNode :red nil "left" nil)
                                  "branch"
                                  (->TreeNode :red nil "right" nil))]
      (is (empty? (children leaf-node)) "Leaf node should return empty list")
      (is (= 2 (count (children branch-node))) "Branch node should return two children"))))

(deftest negative-test-children
  (testing "children should return correct number of children"
    (let [leaf-node (->TreeNode :black nil "leaf" nil)
          left-child-node (->TreeNode :black (->TreeNode :red nil "left" nil) "node" nil)
          right-child-node (->TreeNode :black nil "node" (->TreeNode :red nil "right" nil))
          full-branch-node (->TreeNode :black
                                       (->TreeNode :red nil "left" nil)
                                       "node"
                                       (->TreeNode :red nil "right" nil))]
      (is (empty? (children leaf-node)) "Leaf node should have no children")
      (is (= 1 (count (children left-child-node))) "Node with left child should have one child")
      (is (= 1 (count (children right-child-node))) "Node with right child should have one child")
      (is (= 2 (count (children full-branch-node))) "Node with both children should have two children"))))

(deftest test-get-tree-content
  (testing "get-tree-content function"
    (let [tree (->TreeNode
                 :black
                 (->TreeNode :red nil "B" nil)
                 "D"
                 (->TreeNode :red nil "F" nil))]
      (is (= ["B" "D" "F"] (get-tree-content tree)) "Should return in-order traversal"))))

(deftest negative-test-get-tree-content
  (testing "get-tree-content should handle various tree structures"
    (let [empty-tree nil
          single-node (->TreeNode :black nil "single" nil)
          unbalanced-left-tree (->TreeNode
                                 :black
                                 (->TreeNode :red nil "left" nil)
                                 "root"
                                 nil)
          unbalanced-right-tree (->TreeNode
                                  :black
                                  nil
                                  "root"
                                  (->TreeNode :red nil "right" nil))]
      (is (empty? (get-tree-content empty-tree)) "Empty tree should return empty sequence")
      (is (= ["single"] (get-tree-content single-node)) "Single node tree should return its value")
      (is (= ["left" "root"] (get-tree-content unbalanced-left-tree)) "Left unbalanced tree should return values in-order")
      (is (= ["root" "right"] (get-tree-content unbalanced-right-tree)) "Right unbalanced tree should return values in-order"))))


(deftest test-invert-colors
  (testing "invert-colors function"
    (let [black-node (->TreeNode :black nil "test" nil)
          red-node (->TreeNode :red nil "test" nil)]
      (is (= :red (:color (invert-colors black-node))) "Black node should become red")
      (is (= :black (:color (invert-colors red-node))) "Red node should become black"))))


(deftest test-rotate-right
  (testing "rotate-right function"
    (let [tree (->TreeNode
                 :black
                 (->TreeNode :red
                             (->TreeNode :red nil "LL" nil)
                             "L"
                             nil)
                 "N"
                 nil)]
      (let [rotated (rotate-right tree)]
        (is (= :red (:color rotated)) "Root should become red")
        (is (= "L" (:value rotated)) "Root value should change")
        (is (= :black (:color (:right rotated))) "Right child should be black")))))

(deftest test-rotate-left
  (testing "rotate-left function"
    (let [tree (->TreeNode
                 :black
                 nil
                 "N"
                 (->TreeNode :red
                             nil
                             "R"
                             (->TreeNode :red nil "RR" nil)))]
      (let [rotated (rotate-left tree)]
        (is (= :red (:color rotated)) "Root should become red")
        (is (= "R" (:value rotated)) "Root value should change")
        (is (= :black (:color (:left rotated))) "Left child should be black")))))

(deftest test-insert-val
  (testing "insert-val function"
    (let [initial-tree nil
          first-node (->TreeNode :red nil "first" nil)
          second-node (->TreeNode :red nil "second" nil)
          tree-with-first (insert-val initial-tree first-node)
          tree-with-two-nodes (insert-val tree-with-first second-node)]
      (is (= :black (:color tree-with-first)) "First inserted node should be black")
      (is (= "first" (:value tree-with-first)) "First node value should be correct")
      (is (some? tree-with-two-nodes) "Should be able to insert second node"))))

(deftest test-read-words
  (testing "read-words function"
    (with-redefs [slurp (constantly "Hello world! This is a test.")]
      (let [words (read-words "dummy-file.txt")]
        (is (= ["hello" "world" "this" "is" "a" "test"] words))))))

(deftest test-write-tree-values-to-file
  (testing "write-tree-values-to-file function"
    (let [tree (->TreeNode
                 :black
                 (->TreeNode :red nil "B" nil)
                 "D"
                 (->TreeNode :red nil "F" nil))
          filename "test-output.txt"]
      (write-tree-values-to-file filename tree)
      (is (= ["B" "D" "F"] (first (read-string (slurp filename)))) "File should contain tree values"))))

(deftest test-write-tree-as-tree-to-file
  (testing "write-tree-as-tree-to-file function"
    (let [tree (->TreeNode
                 :black
                 (->TreeNode :red nil "B" nil)
                 "D"
                 (->TreeNode :red nil "F" nil))
          filename "test-tree.txt"]
      (write-tree-as-tree-to-file filename tree)
      (let [read-tree (read-string (slurp filename))]
        (is (= (:value tree) (:value read-tree)) "Read tree should have same value")
        (is (= (:color tree) (:color read-tree)) "Read tree should have same color")))))