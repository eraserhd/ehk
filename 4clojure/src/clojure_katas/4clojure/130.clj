(ns clojure-katas.4clojure.130
  "Tree reparenting

  Every node of a tree is connected to each of its children as
  well as its parent.  One can imagine grabbing one node of
  a tree and dragging it up to the root position, leaving all
  connections intact.  For example, below on the left is
  a binary tree.  By pulling the \"c\" node up to the root, we
  obtain the tree on the right.

  <img src=\"http://i.imgur.com/UtD2T.png\">

  Note it is no longer binary as \"c\" had three connections
  total -- two children and one parent.

  Each node is represented as a vector, which always has at
  least one element giving the name of the node as a symbol.
  Subsequent items in the vector represent the children of the
  node.  Because the children are ordered it's important that
  the tree you return keeps the children of each node in order
  and that the old parent node, if any, is appended on the
  right.

  Your function will be given two args -- the name of the node
  that should become the new root, and the tree to transform.")

(def __
  (fn [new-parent tree]
    (let [tree-contains? (fn [tree node]
                           (some #{node} (flatten tree)))
          subtree-containing (fn [tree node]
                               (first (for [subtree (rest tree)
                                            :when (tree-contains? subtree node)]
                                        subtree)))
          without-subtree-containing (fn [tree node]
                                       (let [node-to-prune (first (subtree-containing tree node))]
                                         (cons (first tree)
                                               (filter #(not= (first %) node-to-prune) (rest tree)))))
          rotate (fn [tree node]
                   (let [subtree (subtree-containing tree node)
                         pruned (without-subtree-containing tree node)]
                     (concat subtree (list pruned))))]
      (loop [tree tree]
        (if (= new-parent (first tree))
          tree
          (recur (rotate tree new-parent)))))))

(= '(n)
   (__ 'n '(n)))
(= '(a (t (e)))
   (__ 'a '(t (e) (a))))
(= '(e (t (a)))
   (__ 'e '(a (t (e)))))
(= '(a (b (c)))
   (__ 'a '(c (b (a)))))
(= '(d 
      (b
        (c)
        (e)
        (a 
          (f 
            (g) 
            (h)))))
  (__ 'd '(a
            (b 
              (c) 
              (d) 
              (e))
            (f 
              (g)
              (h)))))
(= '(c 
      (d) 
      (e) 
      (b
        (f 
          (g) 
          (h))
        (a
          (i
          (j
            (k)
            (l))
          (m
            (n)
            (o))))))
   (__ 'c '(a
             (b
               (c
                 (d)
                 (e))
               (f
                 (g)
                 (h)))
             (i
               (j
                 (k)
                 (l))
               (m
                 (n)
                 (o))))))
