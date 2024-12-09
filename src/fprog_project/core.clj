(ns fprog-project.core
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str])
  (:gen-class)
  (:import (clojure.lang IPersistentSet)))

(def red :red)
(def black :black)

(defrecord TreeNode [color left value right])

(def empty-tree nil)

(def example-tree-left-unbalanced
  (->TreeNode :black (->TreeNode :red (->TreeNode :red nil "Value3" nil) "Value2" nil) "Value" nil))


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

(defn balance-chatgpt
  "Ensures the given subtree stays balanced by rearranging black nodes
  that have at least one red child and one red grandchild"
  [tree]
  (let [tree-map (into {} tree)] ; Convert TreeNode to a map
    (match [tree-map]
           ;; Left child red with left red grandchild
           [{:color :black
             :left {:color :red
                    :left {:color :red, :left a, :value x, :right b}
                    :value y, :right c}
             :value z, :right d}]
           (do
             (println "Match found! Balancing (left-left case):" tree)
             (->TreeNode :red
                         (->TreeNode :black a x b)
                         y
                         (->TreeNode :black c z d)))

           ;; Left child red with right red grandchild
           [{:color :black
             :left {:color :red
                    :left a, :value x
                    :right {:color :red, :left b, :value y, :right c}}
             :value z, :right d}]
           (do
             (println "Match found! Balancing (left-right case):" tree)
             (->TreeNode :red
                         (->TreeNode :black a x b)
                         y
                         (->TreeNode :black c z d)))

           ;; Right child red with left red grandchild
           [{:color :black
             :left a, :value x
             :right {:color :red
                     :left {:color :red, :left b, :value y, :right c}
                     :value z, :right d}}]
           (do
             (println "Match found! Balancing (right-left case):" tree)
             (->TreeNode :red
                         (->TreeNode :black a x b)
                         y
                         (->TreeNode :black c z d)))

           ;; Right child red with right red grandchild
           [{:color :black
             :left a, :value x
             :right {:color :red
                     :left b, :value y
                     :right {:color :red, :left c, :value z, :right d}}}]
           (do
             (println "Match found! Balancing (right-right case):" tree)
             (->TreeNode :red
                         (->TreeNode :black a x b)
                         y
                         (->TreeNode :black c z d)))

           ;; Default case: No balancing needed
           :else
           (do
             (println "No match for subtree:" tree)
             tree))))


(defn match-example
  "trys to match a defrecord"
  [tree]
  (match [tree]
         ;; Match a TreeNode with any values for color, left, value, and right
         [nil] nil
         ;[(:or {:color :red} {:color :black })]
         [{:color :black, :left {:color :red, :left {:color :red}}}]
         (do
           (println "Matched: TreeNode with color black")
           tree) ;; Return the TreeNode
         ;[TreeNode]
         ;(do
         ;  (println "Match")
         ;  tree) ;; Return the TreeNode

         ;; Default case: No match
         :else
         (do
           (println "Not match")
           tree))) ;; Return the TreeNode



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
              (match tree
                     [nil] [:red nil node nil]
                     [TreeNode] (cond
                                  (< node (:value tree)) (balance [(:color) (ins (:left tree)) (:value tree) (:right tree)])
                                  (> node (:value tree)) (balance [(:color) (:left tree) (:value tree) (ins (:right tree))])
                                  :else tree)))
        [_ a y b] (ins tree)]
    [:black a y b]))

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


(def example-tree
  (->TreeNode
    :black
    (->TreeNode :red nil "Funktionale Programmierung" nil)  ;; Left subtree
    "Data Science"
    (->TreeNode :red nil "Informatik" nil)))                ;; Right subtree

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (->>
    ;(read-words "resources/war_and_peace_short.txt")
    ;(read-words "resources/test_file_punctuation_numbers.txt")
    ;(last)
    ;(find-val-node example-tree "Informatik")
    ;(find-val-node empty-tree "Tmp")
    ;(write-tree-to-file example-tree "resources/testing-execution.txt")
    (balance example-tree-left-unbalanced)
    ;(match-example example-tree-left-unbalanced)
    (println)))


