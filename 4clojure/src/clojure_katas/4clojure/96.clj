(ns clojure-katas.4clojure.96
  "Beauty is Symmetry

  Let us define a binary tree as \"symmetric\" if the left half of the tree is
  the mirror image of the right half of the tree.  Write a predicate to
  determine whether or not a given binary tree is symmetric. (see <a
  href='/problem/95'>To Tree, or not to Tree</a> for a reminder on the tree
  representation we're using).")

(def __

  (fn [[_ a b]]
    (letfn [(mirror? [a b]
              (if (and (coll? a) (coll? b))
                (let [[ah al ar] a
                      [bh bl br] b]
                  (and (= ah bh)
                       (mirror? al br)
                       (mirror? ar bl)))
                (= a b)))]
      (mirror? a b)))

  )

(comment
  (= (__ '(:a (:b nil nil) (:b nil nil))) true)
  (= (__ '(:a (:b nil nil) nil)) false)
  (= (__ '(:a (:b nil nil) (:c nil nil))) false)
  (= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
          [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
     true)
  (= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
          [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]])
     false)
  (= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
          [2 [3 nil [4 [6 nil nil] nil]] nil]])
     false))
