(ns fprog-project.compare-insert-time)

(defrecord TreeNode [value left right])

(defn insert-node
  "Inserts a value into the binary tree."
  [tree value]
  (if (nil? tree)
    (->TreeNode value nil nil) ;; Base case: create a new node.
    (if (< (compare value (:value tree)) 0)
      (->TreeNode (:value tree)
                  (insert-node (:left tree) value) ;; Insert into left subtree
                  (:right tree))
      (->TreeNode (:value tree)
                  (:left tree)
                  (insert-node (:right tree) value))))) ;; Insert into right subtree.

(defn sequential-insert
  "Performs sequential insertions into a binary tree."
  [values]
  (reduce insert-node nil values)) ;; Sequentially insert each value

(defn tree-values
  "Returns all values in the tree as a lazy sequence (in-order traversal)."
  [tree]
  (when tree
    (concat (tree-values (:left tree))
            [(:value tree)]
            (tree-values (:right tree)))))

(defn merge-trees
  "Merges two binary trees into one by inserting all values of one into the other."
  [tree1 tree2]
  (reduce insert-node tree1 (tree-values tree2)))

(defn parallel-insert
  "Performs parallel insertions into a binary tree."
  [values]
  (let [chunks (partition-all (/ (count values) 5000) values) ;; Split into 4 chunks
        subtrees (pmap (fn [chunk] (reduce insert-node nil chunk)) chunks)] ;; Create subtrees in parallel
    (reduce merge-trees nil subtrees))) ;; Merge subtrees sequentially

(defn random-string
  "Generates a random string of length n."
  [n]
  (apply str (repeatedly n #(char (+ (rand-int 26) 97)))))

(defn generate-random-strings
  "Generates a list of n random strings."
  [n]
  (repeatedly n #(random-string 10)))

(defn -main []
  ;; Generate 10,000 random strings
  (let [values (do (println "Generating 10,000 random strings...")
                   (time (generate-random-strings 100000)))]

    ;; Sequential Insert
    (println "\nStarting Sequential Insert...")
    (time
      (let [sequential-tree (sequential-insert values)]
        (println "Sequential Insert Done.")
        (println "Sequential Tree Node Count:" (count (tree-values sequential-tree)))))

    ;; Parallel Insert
    (println "\nStarting Parallel Insert...")
    (time
      (let [parallel-tree (parallel-insert values)]
        (println "Parallel Insert Done.")
        (println "Parallel Tree Node Count:" (count (tree-values parallel-tree)))))

    (println "\nExecution Complete."))

  (shutdown-agents))