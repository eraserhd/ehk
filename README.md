Cracker Barrel has a triangular peg solitair game with fifteen holes and
fourteen golf tees.  It looks like this:

FIXME: Image

I wondered how I'd write a solver for this game in Common Lisp.  Hrmm:

## States

A _state_ in this game is one configuration of the board.  For example, the
initial state looks like this:

FIXME: Image

And a possible state after a player's first move looks like this:

FIXME: Image

Since each state is made up of fifteen bits of information (whether each
hole is occupied), we can encode a state as a 15-bit integer.  For example,
a state with a peg in holes 3, 11, and 14 would be `#b100100000001000`
(a prefix of `#b` in Lisp represents a literal binary number).  Converting
this binary number to decimal yields 18440.

This allows us to notice an upper bound to the number of possible states–
something important for designing a solver.  The largest 15-bit number
is 32767.  That's good news, we can store all of them in memory easily,
especially since we now have a numeric encoding for each that we can
use as an array index.

Some of the states can't be reached.  It's impossible to achieve a board
with no pegs on it.  Also, we only have two legal moves on the first
turn, after which we come to one of two possible configurations of the
board having two empty holes–but we have quite a lot of fifteen bit
integers with two zero bits.

So we have our first bit of code:

```lisp
(defvar *start-state* #b111111111111110)
```

And we need to know when we've reached a goal state, which is any state
with just one peg:

```lisp
(defun end-state-p (state)
  (= 1 (logcount state)))
```

LOGCOUNT is a Lisp function which counts the number of bits in an integer.
I wrote it and tested it in the SBCL REPL.

The next thing we need to figure out is how to tell which holes are adjacent
to other holes.  First, to choose a numbering scheme.  The simplest thing
seems to be this:

               0
             1   2
           3   4   5
         6   7   8   9
      10  11  12  13  14


My first thought is that I can keep a logic table of all the triplets
of holes which can represent a jump.  For example `(0 2 5)` and `(7 8 9)`.
That seems ugly, so I reject it.

I consider something I remember about representing a complete binary tree as
an array, scratch around some math, realize that it doesn't apply here, so
I reject this as well.

So then I think the simplest thing I can do is to keep track of sequences
of consecutive holes.  This seems straightforward in Lisp:

```lisp
(defvar *sequences* '((0 2 5 9 14) ; Moving down and right
                      (1 4 8 13)
                      (3 7 12)
                      (0 1 3 6 10) ; Moving down and left
                      (2 4 7 11)
                      (5 8 12)
                      (3 4 5)      ; Moving left-to-right
                      (6 7 8 9)
                      (10 11 12 13 14)))
```

I've removed entries for rows with fewer than three consecutive slots, since
we can't jump a peg with fewer than three slots and I can't think of another
use for the information.

Next, I start coding up a function, NEXT-STATES, that takes an input state
and returns a list of all states reachable from it by making one jump.  I wrote
down a little code, and was interrupted.  When I came back, I noticed
a bunch of simpler steps I could take, so I deleted that and started with
toggling a hole:

```lisp
(defun toggle-hole (state hole-number)
  (lognot (aref state hole-number)))
```
        
And test in the REPL.  It turns out this is wrong, as LOGNOT does not return
1 for 0 and 0 for 1.  In any case, we can use LOGXOR to flip the low bit,
changing 0 to 1 and 1 to 0, fixing our REVERSE-BIT:

```lisp
(defun toggle-hole (state hole-number)
  (logxor 1 (aref state hole-number)))
```

Test this in the REPL, and yay!  Success!

So I'm thinking about this function, and I'm not sure I like it.  Why?  Well,
when I was thinking about how moves work, they reverse three bits at a time.
A move can only be made when the spot to which you jump is empty and the
other two are full, this means that all three holes are "flipped":  The peg
which does the jumping is turned from full to empty, the peg being jumped
turned from full to empty, and the target spot is turned from empty to full.
Thinking about this, I want to flip three bits simulataneously.  So I recode
a bit:

```lisp
(defun toggle-3-holes (state hole-numbers)
  (loop with result = state
        for bit in hole-numbers
	for bit-number from 0 to 2
        do (setf result (logxor (ash 1 bit) result))
	finally (return result)))
```

The BIT-NUMBER part above is a kind of hack.  When writing NEXT-STATES before,
I realized that Lisp's LOOP ... ON is a useful way to iterate over a sequence of
holes.  If we do:

```lisp
(loop for i on '(0 1 3 6 10)
      do (format t "~A~%" i))
```

We get the following output:

    (0 1 3 6 10)
    (1 3 6 10)
    (3 6 10)
    (6 10)
    (10)
    NIL

Limiting the bits reversed to 3 means that we can pass a reference to a sequence
longer than three bits and reverse only the first three bits.

Whew.

Well, the next thing we want to know is whether we can make a jump, given three
consecutive holes.  Let's try:

```lisp
(defun can-jump (state hole-numbers)
  (and (>= (length hole-numbers) 3)
       (logtest state (ash 1 (second hole-numbers)))
       (not (eq (logtest state (ash 1 (first hole-numbers)))
		(logtest state (ash 1 (third hole-numbers)))))))
```

Hah.  The beatiful trick here is this:  The middle hole has to be filled, of
course, and _one_ of the holes on either end must be filled while the other
must be empty.  This catches whether a jump can be made _in either direction_.
Which is pretty cool, because TOGGLE-3-HOLES will make the correct result
regardless of the direction.

A little bit of testing in the REPL, and success!

OK, let's go back to NEXT-STATES and try again now that we have some cool
tools.

```lisp
(defun next-states (state)
  (loop with result = ()
        for sequence in *sequences*
        do (loop for possible-jump on sequence
                 when (can-jump state possible-jump)
                 do (push (toggle-3-holes state possible-jump) result))
        finally (return result)))
```

A few attempts with this in the REPL and it seems to work!

    * (next-states *start-state*)

    (32757 32731)
    * (mapcar #'next-states (next-states *start-state*))

    ((32717 31677 28541 32487) (32739 28411 32591 15867))

So now what?

What we want to accomplish is finding a sequence of moves which wins the game.
We have the starting board configuration.  We know how to compute, given one
board configuration, all the possible board configurations after the next move.

Normally, I'd do this with something like a depth-first search or a
breadth-first search.  I almost always prefer breadth-first search, but as
I think about it, a neat kind of variation on BFS appears where each level
is computed successively.  I mean, we start with the start state, then
we figure out all the possible states after one move, then all the possible
states after two moves.

```lisp
(defun solve ()
  (let ((states (list *start-state*)))
    (loop
      (if (find-if #'end-state-p states)
        (return t))
      (setf states (mapcan #'next-states states))
      (if (null states)
        (return nil)))))
```

This is the first draft.  It only recursively makes moves until it finds
a solution.  It returns T if it does, or NIL if it does not.  It returns
T.  I don't have a very high level of confidence that the code is correct,
though it's comforting that it takes about three seconds to run on my
laptop.

The next part is wanting to find a sequence of moves that solves the
problem.  This is where the bit representation and the array stuff comes
in handy.  I want to add an array which can point from each state to its
previous state.  This can be used to trace the path backward from
a found goal to the first move.

```lisp
(defun solve ()
  (let ((states (list *start-state*))
        (found (make-array 32768 :initial-element -1))
        (states-for-next-iteration ()))
    (loop
      (setf states-for-next-iteration ())

      (dolist (state states)
        (if (end-state-p state)
          (let ((path ()))
            (loop while (not (= state -1))
                  do (push state path)
                     (setf state (aref found state)))
            (return-from solve path)))

        (dolist (next-state (next-states state))
          (if (= -1 (aref found next-state))
            (progn
              (setf (aref found next-state) state)
              (push next-state states-for-next-iteration)))))
      (setf states states-for-next-iteration)
      (if (null states)
        (return-from solve nil)))))
```

Writing this was kind of painful.  At first, it simply printed
NIL.  I restructured some, kind of without understanding why it
didn't work and it again printed T.  Then using the array sped
up the computation (as expected - states are no longer visited
more than once).

Finally, code was added to reconstruct the path and return it
when a goal state was found.  I put this inline and reused a
variable (yuck).  It gives the following output:

    (32766 32731 32739 31659 31713 27497 7017 9065 8995 8455 23 50 10 64)

Well... Huh.  What does it mean?

So we need a function to print a state in a way we can understand.

```lisp
(defvar *template*
  (concatenate 'string
    "    ~A~%"
    "   ~A ~A~%"
    "  ~A ~A ~A~%"
    " ~A ~A ~A ~A~%"
    "~A ~A ~A ~A ~A~%"
    "~%"))

(defun print-state (state)
  (apply #'format t *template*
         (loop for i from 0 to 14
               collect (if (logtest (ash 1 i) state)
                         "X"
                         "."))))
```

Now we can run the following from the REPL:

```lisp
(mapc #'print-state (solve))
```

And we get the following output:

        .
       X X
      X X X
     X X X X
    X X X X X

        X
       X .
      X X .
     X X X X
    X X X X X

        X
       X .
      . . X
     X X X X
    X X X X X

        X
       X .
      X . X
     . X X X
    . X X X X

        X
       . .
      . . X
     X X X X
    . X X X X

        X
       . .
      X . X
     X . X X
    . X . X X

        X
       . .
      X . X
     X . X X
    . X X . .

        X
       . .
      X . X
     X . X X
    . . . X .

        X
       X .
      . . X
     . . X X
    . . . X .

        X
       X X
      . . .
     . . X .
    . . . X .

        X
       X X
      . X .
     . . . .
    . . . . .

        .
       X .
      . X X
     . . . .
    . . . . .

        .
       X .
      X . .
     . . . .
    . . . . .

        .
       . .
      . . .
     X . . .
    . . . . .

