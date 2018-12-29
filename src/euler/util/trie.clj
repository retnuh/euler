(ns euler.util.trie
  (:import [java.util HashMap])
  (:use [clojure.pprint     :only (pprint)]))

;;
;; Trie, but not purely functional.
;;

;;
;; Using protocols
;;


(defprotocol Trie
  (is-word?     [this])
  (insert!      [this word])
  (find-prefix  [this prefix])
  (all-children [this prefix]))

(defrecord TrieNode [word? children letter])

(defn make-node [word? letter & [children]]
  (->TrieNode word? (or children (HashMap.)) letter))

(defn- create! [children letter word? & [existing-children]]
    (let [child (make-node word? letter existing-children)]
      (.put children letter child)
      child))


(defn- create-or-update! [children letter word?]
  (if-let [child (.get children letter)]
    (if (and word? (not (is-word? child)))
      (create! children letter word? (:children child))
      child)
    (create! children letter word?)))


(extend-protocol Trie
  nil
  (is-word? [_] false)
  (insert! [_] nil)
  (find-prefix [_] nil)
  (all-children [_ prefix] [])
  
  TrieNode
  (is-word? [this]
    (:word? this))

  (insert! [this word]
    (if (> (count word) 1)
      (let [child (create-or-update! (:children this) (first word) false)]
        (insert! child (rest word)))
      (create-or-update! (:children this) (first word) true)))

  (find-prefix [this prefix]
    (when-let [child (.get (:children this) (first prefix))]
      (if (> (count prefix) 1)
        (find-prefix child (rest prefix))
        child)))

  (all-children [this prefix]
    (let [so-far (concat prefix [(:letter this)])
          chillins (mapcat #(all-children % so-far) (.values (:children this)))]
        (if (is-word? this) 
            (concat [(apply str so-far)] chillins)
            chillins))))


