(ns fprog-project.core
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str])
  (:gen-class))

(defn balance
  "Ensures the given subtree stays balanced by rearranging black nodes
  that have at least one red child and one red grandchild"
  [tree]
  (match [tree]
         [(:or ;; Left child red with left red grandchild
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
  Returns a node with x and no children if tree is nil.

  Returned tree is balanced. See also `balance`"
  [tree x]
  (let [ins (fn ins [tree]
              (match tree
                     nil [:red nil x nil]
                     [color a y b] (cond
                                     (< x y) (balance [color (ins a) y b])
                                     (> x y) (balance [color a y (ins b)])
                                     :else tree)))
        [_ a y b] (ins tree)]
    [:black a y b]))

(defn find-val
  "Finds value x in tree"
  [tree x]
  (match tree
         nil nil
         [_ a y b] (cond
                     (< x y) (recur a x)
                     (> x y) (recur b x)
                     :else x)))


(defn read-words
  "Reads a file in a str without punctuation or numbers"
  [filename]
  (->>
    (slurp filename)
    (str/split-lines)
    (remove str/blank?)
    (apply str)
    (str/trim)
    (remove #(not (or (Character/isLetter %) (Character/isWhitespace %))))
  ))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (->>
    (read-words "resources/war_and_peace_short.txt")
    (println)
    )
  )
