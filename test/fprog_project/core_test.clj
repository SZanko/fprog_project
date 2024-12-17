(ns fprog-project.core-test
  (:require [clojure.test :refer :all]
            [fprog-project.core :refer :all]))


(deftest test-get-tree-content
  (testing "get-tree-content function"
    (let [tree (->TreeNode
                 :black
                 (->TreeNode :red nil "B" nil)
                 "D"
                 (->TreeNode :red nil "F" nil))]
      (is (= ["B" "D" "F"] (get-tree-content tree)) "Should return in-order traversal"))))



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


(deftest test-rotation-preservation
  (testing "Rotations preserve tree properties"
    (let [tree (->TreeNode
                 :black
                 (->TreeNode :red
                             (->TreeNode :red nil "LL" nil)
                             "L"
                             (->TreeNode :red nil "LR" nil))
                 "N"
                 nil)
          rotated-right (rotate-right tree)]

      ;; Verify values are preserved after rotation
      (is (some #(= "LL" %) (get-tree-content rotated-right)) "Rotation should preserve all values")
      (is (some #(= "L" %) (get-tree-content rotated-right)) "Rotation should preserve all values")

      ;; Check color changes
      (is (= :red (:color rotated-right)) "Right rotation should change root color")
      (is (= :black (:color (:right rotated-right))) "Right rotation should blacken appropriate nodes"))))



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


