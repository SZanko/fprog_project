(ns fprog-project.core
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str])
  (:gen-class))

(def red :red)
(def black :black)

(defrecord TreeNode [color left value right])

(def empty-tree nil)

(def example-tree-no-children
  (->TreeNode :black nil "Value" nil))

(defn create-node-right
  [value right]
  (->TreeNode nil nil value right)
  )

(def example-tree-left-unbalanced-left-grandchild
  (->TreeNode :black (->TreeNode :red (->TreeNode :red nil "LL" nil) "L" nil) "N" nil))

(def example-tree-left-unbalanced-right-grandchild
  (->TreeNode :black (->TreeNode :red nil "L" (->TreeNode :red nil "LR" nil)) "N" (->TreeNode :red nil "R" nil)))

(def example-tree-right-unbalanced-right-grandchild
  (->TreeNode :black nil "N" (->TreeNode :red nil "R" (->TreeNode :red nil "RR" nil))))

(def example-tree-right-unbalanced-left-grandchild
  (->TreeNode :black (->TreeNode :red (->TreeNode :black nil "LL" nil) "L" nil) "N" (->TreeNode :red (->TreeNode :red nil "RL" nil) "R" nil)))

(def example-tree
  (->TreeNode
    :black
    (->TreeNode :red nil "Funktionale Programmierung" nil)         ;; Left subtree
    "Data Science"
    (->TreeNode :red nil "Informatik" nil)))                       ;; Right subtree



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
  (pmap :value (tree-seq branch? children tree)))

; todo fix recolor
(defn rotate-right
  "rotates the tree right"
  [^TreeNode tree]
  ;(println "rotate right")
  (let [left-grand-child (when (:value (:left (:left tree)))
                           (->TreeNode
                             :red
                             (:left  (:left (:left tree)))
                             (:value (:left (:left tree)))
                             (:right (:left (:left tree))))
                           )]
    (->TreeNode
      (:color tree)
      (or left-grand-child nil)
      (:value (:left tree))
      (->TreeNode
        :red
        (:right (:left tree))
        (:value tree)
        (:right tree))
      )
    )
  )

; todo fix recolor
(defn rotate-left
  "rotates the tree left"
  [^TreeNode tree]
  ;(println "rotate left")
  (let [right-grand-child (when (:value (:right (:right tree)))
                            (->TreeNode
                              :red
                              (:left (:right (:right tree)))
                              (:value (:right (:right tree)))
                              (:right (:right (:right tree))))
                            )]
      (->TreeNode
        (:color tree)
        (->TreeNode
          :red
          (:left tree)
          (:value tree)
          (:left (:right tree)))
        (:value (:right tree))
        (or right-grand-child nil)
      )
    )
  )


(defn balance
  "Ensures the given subtree stays balanced by rearranging black nodes
  that have at least one red child and one red grandchild"
  [^TreeNode tree]
  (match [tree]
         ;; Left child red with left red grandchild
         [(:or {:color :black, :left {:color :red, :left {:color :red}}}
            ;;; Left child red with right red grandchild
            {:color :black, :left {:color :red, :right {:color :red}}})]
         (rotate-right tree)

         ;; Right child red with left red grandchild
         [{:color :black, :right {:color :red, :left {:color :red}}}]
         (rotate-left tree)

         ;; Right child red with right red grandchild
         [{:color :black, :right {:color :red, :right {:color :red}}}]
         (rotate-left tree)

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
  [^String filename ^TreeNode tree]
  (->> (get-tree-content tree)
       (list)
       (spit filename)))

(defn parallel-insert
  "Inserts nodes into a red-black tree in parallel."
  [nodes]
  (let [chunk-size 500  ;; Adjust based on your system's capacity
        chunks (partition-all chunk-size nodes)]  ;; Step 1: Split nodes into chunks
    ;; Step 2: Create subtrees in parallel
    (let [subtrees (pmap
                     (fn [chunk]
                       (reduce (fn [tree node] (insert-val tree node)) nil chunk))
                     chunks)]
      ;; Step 3: Merge subtrees sequentially
      (reduce (fn [tree subtree]
                (reduce (fn [tree node] (insert-val tree node))
                        tree
                        (get-tree-content subtree)))
              nil
              subtrees))))

(defn -main
  "Reads file converts words to nodes and builds a red and black tree"
  [& args]
  ;(time
  ;(->>
  ;  (read-words "resources/war_and_peace.txt")
  ;  (pmap #(->TreeNode :red nil % nil))
  ;  (reduce (fn [tree node] (insert-val tree node)) nil) ; todo use parallelization for that
  ;  (write-tree-to-file "resources/testing-execution.txt")
  ;  ))
  (time
    (let [words (do
                  (println "Reading words from file...")
                  (time (read-words "resources/war_and_peace.txt")))

          nodes (do
                  (println "Creating tree nodes...")
                  (time (map #(->TreeNode :red nil % nil) words)))

          tree  (do
                  (println "Inserting nodes into the tree...")
                  (time (parallel-insert nodes)))  ; todo use parallelization for that

          _     (do
                  (println "Writing tree to file...")
                  (time (write-tree-to-file "resources/testing-execution.txt" tree)))]))

  (println "Finished")
  (shutdown-agents) ; shuts down the thread pool
  )