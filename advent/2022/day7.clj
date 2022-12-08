(ns day7
 (:require
  [nextjournal.clerk :as clerk]))

;; ## Day 7: No Space Left On Device
;;
;; > You can hear birds chirping and raindrops hitting leaves as the expedition
;; proceeds. Occasionally, you can even hear much louder sounds in the distance;
;; how big do the animals get out here, anyway?
;;
;; > The device the Elves gave you has problems with more than just its
;; communication system. You try to run a system update:
;;
;;     $ system-update --please --pretty-please-with-sugar-on-top
;;     Error: No space left on device
;;     Perhaps you can delete some files to make space for the update?
;;
;; > You browse around the filesystem to assess the situation and save the
;; resulting terminal output (your puzzle input). For example:

^{::clerk/visibility {:code :hide :result :hide}}
(def example-terminal-output "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

^{::clerk/visibility {:code :hide}}
(clerk/html [:pre example-terminal-output])

;; > The filesystem consists of a tree of files (plain data) and directories
;; (which can contain other directories or files). The outermost directory is
;; called /. You can navigate around the filesystem, moving into or out of
;; directories and listing the contents of the directory you're currently in.
;;
;; > Within the terminal output, lines that begin with $ are commands you
;; executed, very much like some modern computers:
;;
;; Luckily, all the tokens including the `$` are nicely readable by the Clojure
;; reader.  We put brackets around everything to read it as a single list of
;; tokens.  We then split the token list at `$` and remove the `($)` lists.
(defn parse-terminal-output [text]
  (->> (read-string (str "(" text ")"))
       (partition-by '#{$})
       (filter (complement '#{($)}))))
(def command-forms (parse-terminal-output example-terminal-output))

;; Now we need an interpreter, so let's define our initial state and
;; make a multimethod to interpret command outputs:

(def initial-state {:cwd [], :files {}})
(defmulti interpret
  (fn [state command & args]
    command))

;; > - cd means change directory. This changes which directory is the current
;;     directory, but the specific result depends on the argument:
;; >   - cd x moves in one level: it looks in the current directory for the directory named x and makes it the current directory.
;; >   - cd .. moves out one level: it finds the directory that contains the current directory, then makes that directory the current directory.
;; >   - cd / switches the current directory to the outermost directory, /.

(defmethod interpret 'cd
  [{:keys [cwd], :as state} _ cd-arg]
  (assoc state :cwd (case cd-arg
                      /  []
                      .. (pop cwd)
                      (conj cwd cd-arg))))

;; Examples:
(-> initial-state
    (interpret 'cd 'foo))
(-> {:cwd '[foo bar baz]}
    (interpret 'cd '..))
(-> {:cwd '[foo bar baz]}
    (interpret 'cd '/))

;; > - ls means list. It prints out all of the files and directories immediately contained by the current directory:
;; >    - 123 abc means that the current directory contains a file named abc with size 123.
;; >    - dir xyz means that the current directory contains a directory named xyz.

(defmethod interpret 'ls
  [{:keys [cwd files] :as state} _ & ls-output]
  (reduce
    (fn [state [size name]]
      (assoc-in state (concat [:files] cwd [name]) (if (= size 'dir)
                                                     {}
                                                     size)))
    state
    (partition 2 ls-output)))

(-> initial-state
    (interpret 'ls
     1234 'foo.txt
     4567 'bar.txt))

(-> {:cwd ["foo" "bar"]}
    (interpret 'ls
     'dir 'x
     106 'y
     107 'z))

;; We can then simulate all the commands:

(defn interpret-all [command-forms]
  (reduce
    (fn [state command-form]
      (apply interpret state command-form))
    initial-state
    command-forms))
(def example-final-state (interpret-all command-forms))

;; > Given the commands and output in the example above, you can determine that the filesystem looks visually like this:

^{::clerk/visibility {:code :hide :result :hide}}
(def example-result "  - a (dir)
    - e (dir)
      - i (file, size=584)
    - f (file, size=29116)
    - g (file, size=2557)
    - h.lst (file, size=62596)
  - b.txt (file, size=14848514)
  - c.dat (file, size=8504156)
  - d (dir)
    - j (file, size=4060174)
    - d.log (file, size=8033020)
    - d.ext (file, size=5626152)
    - k (file, size=7214296)")

^{::clerk/visibility {:code :hide}}
(clerk/html [:pre example-result])

;; > Here, there are four directories: / (the outermost directory), a and d
;; (which are in /), and e (which is in a). These directories also contain files
;; of various sizes.

;; > Since the disk is full, your first step should probably be to find
;; directories that are good candidates for deletion. To do this, you need to
;; determine the total size of each directory. The total size of a directory is
;; the sum of the sizes of the files it contains, directly or indirectly.
;; (Directories themselves do not count as having any intrinsic size.)

;; > The total sizes of the directories above can be found as follows:

(defn total-size [files]
  (cond
   (map? files)
   (->> (vals files)
        (map total-size)
        (reduce +))

   (number? files)
   files))

;; > - The total size of directory e is 584 because it contains a single file i of size 584 and no other directories.

(= 584 (total-size (get-in example-final-state [:files 'a 'e])))

;; > - The directory a has total size 94853 because it contains files f (size 29116), g (size 2557), and h.lst (size 62596), plus file i indirectly (a contains e which contains i).

(= 94853 (total-size (get-in example-final-state [:files 'a])))

;; > - Directory d has total size 24933642.

(= 24933642 (total-size (get-in example-final-state [:files 'd])))

;; > - As the outermost directory, / contains every file. Its total size is 48381165, the sum of the size of every file.

(= 48381165 (total-size (:files example-final-state)))

;; > To begin, find all of the directories with a total size of at most
;; 100000, then calculate the sum of their total sizes. In the example above,
;; these directories are a and e; the sum of their total sizes is 95437 (94853 +
;; 584). (As in this example, this process can count files more than once!)

(defn all-directories [path files]
  (concat [path]
          (->> files
               (filter #(map? (val %)))
               (mapcat (fn [[dirname subtree]]
                         (all-directories (conj path dirname) subtree))))))

(all-directories [] '{foo {bar {baz 45}}})

(defn sizes-under [files n]
  (for [d (all-directories [] files)
        :let [size (total-size (get-in files d))]
        :when (< size n)]
    size))

(sizes-under (:files example-final-state) 100000)

(defn solve [text]
  (let [final-state (interpret-all (parse-terminal-output text))
        sizes       (sizes-under (:files final-state) 100000)]
    (reduce + sizes)))

(= 95437 (solve example-terminal-output))

;; Find all of the directories with a total size of at most 100000. What is the sum of the total sizes of those directories?

(solve (slurp "day7.txt"))
