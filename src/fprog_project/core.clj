(ns fprog-project.core
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str])
  (:gen-class)
  (:import (clojure.lang IPersistentSet)))

(def red :red)
(def black :black)

(defrecord TreeNode [color left value right])

(def empty-tree nil)

(def example-tree-no-children
  (->TreeNode :black nil "Value" nil))

(def example-tree-left-unbalanced
  (->TreeNode :black (->TreeNode :red (->TreeNode :red nil "Value3" nil) "Value2" nil) "Value" nil))

(def example-tree
  (->TreeNode
    :black
    (->TreeNode :red nil "Funktionale Programmierung" nil)  ;; Left subtree
    "Data Science"
    (->TreeNode :red nil "Informatik" nil)))                ;; Right subtree

(defn is-red-node
  "Checks if the node is red"
  [^TreeNode node]
  (->> (:color node)
       (= red)
       (and node)))


(defn is-black-node
  "Checks if the node is black"
  [^TreeNode node]
  (->> (:color node)
       (= black)
       (and node)))

(defn branch? [^TreeNode node]
  "Define a predicate function to determine if a node has children"
  (and node (or (:left node) (:right node))))

(defn children [^TreeNode node]
  "Define a function to retrieve the children of a node"
  (filter some? [(:left node) (:right node)]))

(defn get-tree-content
  "get the content of a tree as seq"
  [^TreeNode tree]
  (map :value (tree-seq branch? children tree)))

(defn match-example
  "trys to match a defrecord"
  [tree]
  (match [tree]
         ;; Match a TreeNode with any values for color, left, value, and right
         [nil] (do (println "Matched: nil") nil)
         ;[(:or {:color :red} {:color :black })]
         ;[{:color :black, :left {:color :red, :left {:color :red}}}]
         [TreeNode]
         (do
           (println "Matched: TreeNode with color black")
           tree)                                            ;; Return the TreeNode
         ;[TreeNode]
         ;(do
         ;  (println "Match")
         ;  tree) ;; Return the TreeNode

         ;; Default case: No match
         :else
         (do
           (println "Not match")
           tree)))                                          ;; Return the TreeNode



(defn balance
  "Ensures the given subtree stays balanced by rearranging black nodes
  that have at least one red child and one red grandchild"
  [tree]
  (match [tree]
         [(:or
            ;;; Left child red with left red grandchild
            {:color :black, :left {:color :red, :left {:color :red}}}
            ;;[:black [:red [:red a x b] y c] z d]
            ;;; Left child red with right red grandchild
            {:color :black, :left {:color :red, :right {:color :red}}}
            ;;[:black [:red a x [:red b y c]] z d]
            ;;; Right child red with left red grandchild
            {:color :black, :right {:color :red, :left {:color :red}}}
            ;;[:black a x [:red [:red b y c] z d]]
            ;;; Right child red with right red grandchild
            {:color :black, :right {:color :red, :right {:color :red}}}
            ;;[:black a x [:red b y [:red c z d]]]
            )]
         ;; if matched what should be done
         {:color :red, :left {:color :black}, :right {:color :black}}
         ;[:red [:black a x b]
         ;y
         ;[:black c z d]]
         :else tree)
  )


(defn insert-val
  "Inserts x in tree.
  Returns a node with node and no children if tree is nil.

  Returned tree is balanced. See also `balance`"
  [^TreeNode tree ^TreeNode node]
  (let [ins (fn ins [tree]
              (match [tree]
                     ;[nil] [:red nil node nil]
                     [nil]
                     (->TreeNode :red nil (:value node) nil)
                     [TreeNode]
                     (let [color (:color tree)
                           left (:left tree)
                           value (:value tree)
                           right (:right tree)]

                       (cond
                         (< (compare (:value node) value) 0)
                          (balance (->TreeNode color (ins left) value right))
                          ;(balance [(:color) (ins (:left tree)) (:value tree) (:right tree)])
                         (> (compare (:value node) value) 0)
                          (balance (->TreeNode color left value (ins right)))
                          ;(balance [(:color) (:left tree) (:value tree) (ins (:right tree))])
                         :else tree))
                       :else (println "No match"))
                       )
        result (ins tree)]
    (assoc result :color :black)))


(defn find-val-node
  "Finds node with value x in tree"
  [^TreeNode tree x]
  (match [tree]
         [nil] nil                                          ;; Match when tree is nil
         [TreeNode]
         (cond
           (< (compare x (:value tree)) 0) (recur (:left tree) x) ;; Search left subtree
           (> (compare x (:value tree)) 0) (recur (:right tree) x) ;; Search right subtree
           :else tree)))                                    ;; Found the value



(defn read-words
  "Reads a file in a str without punctuation or numbers"
  [^String filename]
  (->>
    (slurp filename)
    (#(str/replace % #"[^a-zA-Z]" " "))
    (#(str/split % #"\s+"))))


(defn write-tree-to-file
  "writes the tree to a file"
  [^TreeNode tree ^String filename]
  (->> (get-tree-content tree)
       (list)
       (spit filename)))



(defn -main
  "Reads file converts words to nodes and builds a red and black tree"
  [& args]
  (->>
    ;(read-words "resources/war_and_peace_short.txt")
    ;(read-words "resources/test_file_punctuation_numbers.txt")
    ;(last)
    ;(find-val-node example-tree "Informatik")
    ;(find-val-node empty-tree "Tmp")
    ;(write-tree-to-file example-tree "resources/testing-execution.txt")
    ;(balance example-tree-left-unbalanced)
    ;(match-example example-tree-left-unbalanced)
    ;(match-example empty-tree)
    (insert-val empty-tree (->TreeNode nil nil "Value" nil))
    (insert-val (->TreeNode nil nil "a" nil))
    ;(insert-val example-tree-no-children (->TreeNode nil nil "Value2" nil))
    ;(insert-val example-tree (->TreeNode nil nil "Value" nil))
    (println)))


