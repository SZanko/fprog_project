(ns fprog-project.core
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str])
  (:gen-class)
  (:import (clojure.lang IPersistentSet)))

(def red :red)
(def black :black)

(defrecord TreeNode [color left value right])

(def empty-tree nil)

(defn is-red-node
  "Checks if the node is red"
  [^TreeNode node]
  (->> (:color node)
       (= red)
       (and node)
       ))

(defn is-black-node
  "Checks if the node is black"
  [^TreeNode node]
  (->> (:color node)
       (= black)
       (and node)
       ))

(defn balance
  "Ensures the given subtree stays balanced by rearranging black nodes
  that have at least one red child and one red grandchild"
  [tree]
  (match [tree]
         [(:or                                              ;; Left child red with left red grandchild
            [:black [:red [:red a x b] y c] z d]
            ;; Left child red with right red grandchild
            [:black [:red a x [:red b y c]] z d]
            ;; Right child red with left red grandchild
            [:black a x [:red [:red b y c] z d]]
            ;; Right child red with right red grandchild
            [:black a x [:red b y [:red c z d]]])] [:red [:black a x b]
                                                    y
                                                    [:black c z d]]
         :else tree))

(defn insert-val
  "Inserts x in tree.
  Returns a node with node and no children if tree is nil.

  Returned tree is balanced. See also `balance`"
  [^TreeNode tree ^TreeNode node]
  (let [ins (fn ins [tree]
              (match tree
                     [nil] [:red nil node nil]
                     [color a value b] (cond
                                         (< node value) (balance [color (ins a) value b])
                                         (> node value) (balance [color a value (ins b)])
                                         :else tree)))
        [_ a y b] (ins tree)]
    [:black a y b]))

(defn find-val-node
  "Finds node with value x in tree"
  [tree x]
  (match [tree]
         [nil] nil ;; Match when tree is nil
         [TreeNode]
         (cond
           (< (compare x (:value tree)) 0) (recur (:left tree) x)  ;; Search left subtree
           (> (compare x (:value tree)) 0) (recur (:right tree) x) ;; Search right subtree
           :else tree)))                         ;; Found the value



(defn read-words
  "Reads a file in a str without punctuation or numbers"
  [filename]
  (->>
    (slurp filename)
    (#(str/replace % #"[^a-zA-Z]" " "))
    (#(str/split % #"\s+"))
    ))

(def example-tree
  (->TreeNode
   :black
        (->TreeNode :red nil "Funktionale Programmierung" nil)                              ;; Left subtree
        "Data Science"
        (->TreeNode :red nil "Informatik" nil)))                          ;; Right subtree

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (->>
    ;(read-words "resources/war_and_peace_short.txt")
    ;(read-words "resources/test_file_punctuation_numbers.txt")
    ;(last)
    (find-val-node example-tree "Informatik")
    ;(find-val-node empty-tree "Tmp")
    (println)
    )
  )
