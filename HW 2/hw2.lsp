; Ethan Wong
; UID 305319001
; DIS 1C

; #1
; BFS - perform a left-to-right Breadth-First Search of a tree
; Arguments: TREE (a list representation of a tree)
; Returns: a top-level list of the terminal nodes in the order they would be visited by a left-to-right breadth-first search

(defun BFS(TREE)
  ; if TREE is empty there's nothing to search through, just return NIL
  (cond ((null TREE) NIL)
        ; if the head of TREE is an atom, we can note it down and then recurse on the rest of the list using BFS, appending the two together
        ((atom (first TREE)) (append (list (first TREE)) (BFS (rest TREE))))
        ; else, move it to the end of the tree so we visit it after visting all the other nodes in the level
        (t (BFS (append (rest TREE) (first TREE))))
  )
)

; Tests - all tests should output T
; (print "BFS TESTS")
; (print (equal (BFS '((A B) (C D))) '(A B C D)))
; (print (equal (BFS '()) '()))
; (print (equal (BFS '(A (B C) (D) (E (F G)))) '(A B C D E F G)))
; (print (equal (BFS '((A (B)) C (D))) '(C A D B)))
; (print (equal (BFS '(A (B))) '(A B)))
; (print (equal (BFS '((1 (2 3)) ((4 5) (6 7)))) '(1 2 3 4 5 6 7)))





; #2
; DFS - perform a left-to-right Depth-First Search of a tree
; Arguments: TREE (a list representation of a tree)
; Returns: a top-level list of the terminal nodes in the order they would be visited by a left-to-right depth-first search

(defun DFS(TREE)
  ; if TREE is empty there's nothing to search through, just return NIL
  (cond ((null TREE) NIL)
        ; if TREE is an atom (one element), return it as a list
        ((atom TREE) (list TREE))
        ; else (TREE is a list), recursively call DFS on first of TREE and rest of TREE (left-to-right), then append together
        (t (append (DFS (first TREE)) (DFS (rest TREE))))
  )
)

; Tests - all tests should output T
; (print "DFS TESTS")
; (print (equal (DFS '()) '()))
; (print (equal (DFS '((A (B)) C (D))) '(A B C D)))
; (print (equal (DFS '((W X) (Y Z))) '(W X Y Z)))
; (print (equal (DFS '(A)) '(A)))
; (print (equal (DFS '((A (B)) C (D (E)))) '(A B C D E)))
; (print (equal (DFS '(A (B C) (D) (E (F G)))) '(A B C D E F G)))




; #3
; set of functions that implement depth-first iterative-deepening
; DFID-helper - a helper function to DFID that performs limited DFS 
; Arguments: TREE (a list representation of a tree), DEPTH (an integer that represents max depth of the tree)
; Returns: a list of the terminal nodes in the order that they would be visited by a right-to-left limited depth-first search
; DFID - top-level function that implements depth-first iterative-deepening
; Arguments: TREE (a list representation of a tree), DEPTH (an integer that represents max depth of the tree)
; Returns: a top-level list of the terminal nodes in the order that they would be visited by a right-to-left depth-first iterative-deepening search

(defun DFID-helper(TREE DEPTH)
  ; if TREE is null, there's nothing to search - return NIL
  (cond ((null TREE) NIL)
        ; if DEPTH is less than 0, then there is nothing to look at - return NIL
        ((< DEPTH 0) NIL)
        ; if TREE is an atom (one element), return it as a list
        ((atom TREE) (list TREE))
        ; else (TREE is a list), recursively call DFID-helper on rest of TREE and first of TREE (right-to-left), decrementing DEPTH every time we go down a level, then append together
        (t (append (DFID-helper (rest TREE) DEPTH) (DFID-helper (first TREE) (- DEPTH 1))))
  )
)

(defun DFID(TREE DEPTH)
  ; if TREE is null, there's nothing to search - return NIL
  (cond ((null TREE) NIL)
        ; if DEPTH is less than 0, then there is nothing to look at - return NIL
        ((< DEPTH 0) NIL)
        ; else (TREE is a list), recursively call DFID-helper on rest of TREE and first of TREE (right-to-left), decrementing DEPTH every time we go down a level, then append together
        (t (append (DFID TREE (- DEPTH 1)) (DFID-helper TREE DEPTH)))
  )
)

; Tests - all tests should output T
; (print "DFID TESTS")
; (print (equal (DFID '(A (B C) (D) (E (F G))) 3) '(A E D C B A G F E D C B A)))
; (print (equal (DFID '() 1) '()))
; (print (equal (DFID '((W X) (Y Z)) 1) NIL))
; (print (equal (DFID '((1 (2 3)) ((4 5) (6 7))) 1) NIL))
; (print (equal (dfid '((A (B)) C (D)) 3) '(C D C A D C B A)))
; (print (equal (DFID '(A B) 2) '(B A B A)))
; (print (equal (DFID '(A) 1) '(A)))




; #4
; solver for the River-Boat Problem
; there are multiple functions involved in this solver - they will be explained one-by-one

; FINAL-STATE - returns whether we are at the final state of (3 3 NIL)
; Arguments - s, a state (which is a list)
; Returns: Boolean (t or NIL)

(defun final-state(s)
  ; return t if s == (3 3 NIL), otherwise return NIL
  (equal s '(3 3 NIL))
)

; Tests - all tests should output T
; (print "FINAL-STATE TESTS")
; (print (equal (final-state '()) NIL))
; (print (equal (final-state '(3 3 NIL)) t))
; (print (equal (final-state '(3 3 T)) NIL))
; (print (equal (final-state '(3 3 3)) NIL))





; NEXT-STATE - returns a list containing the successor state (which is itself a list)
; Arguments - s (current state), m (number of X's to move), c (number of O's to move)
; Returns - the state (a list) that results from moving that number of X's and O's from the current side of the river to the other side of the river; NIL if invalid move

(defun next-state(s m c)
  ; get stats from the current state (number of X's and number of O's on the current side of the river)
  (let* ((CURR-X (first s)) (CURR-O (second s)) (CURR-SIDE (third s)) 
        ; get stats from the other side of current state (number of X's and number of O's on the other side of the river)
        (OTHER-X (- 3 CURR-X)) (OTHER-O (- 3 CURR-O)) (OTHER-SIDE (not CURR-SIDE)))

    ; check for any invalid states/actions
    (cond 
        ; (number of X's to move) + (number of O's to move) > 2  --  (more than 2 people trying to get on the boat)
        (
          (or (> (+ m c) 2)
            ; (number of O's on current side of river) < (number of O's to move)  --  (trying to move more people than we actually have)
            (< CURR-O c)
            ; (number of X's on current side of river) < (number of X's to move)  --  (trying to move more people than we actually have)
            (< CURR-X m)
            ; after moving the boat, there are more O's than X's on other side of the river (assuming there are any X's on that side)
            (and (> (+ OTHER-X m) 0) (> (+ OTHER-O c) (+ OTHER-X m)))
            ; after moving the boat, there are more O's than X's on current side of the river (assuming there are any X's on this side)
            (and (> (- CURR-X m) 0) (> (- CURR-O c) (- CURR-X m)))  
          ) NIL
        )
        ; else (no invalid states/actions occured) -- create the succesor state (all the people who needed to move were moved successfully)
        (t (list (list (+ OTHER-X m) (+ OTHER-O c) OTHER-SIDE)))
    )
  )
)

; Tests - all tests should output T
; (print "NEXT-STATE TESTS")
; (print (equal (next-state '(3 3 t) 1 0) NIL))
; (print (equal (next-state '(3 3 t) 0 1) '((0 1 NIL))))
; (print (equal (next-state '(1 1 t) 1 1) '((3 3 NIL))))
; (print (equal (next-state '(3 3 t) 2 0) NIL))
; (print (equal (next-state '(1 0 t) 1 0) '((3 3 NIL))))
; (print (equal (next-state '(2 2 t) 1 4) NIL))

; SUCC-FN - returns all of the possible legal successor states to the current state
; Arguments - s (current state)
; Returns - list of states that can be reached by applying legal operators to s

(defun succ-fn(s)
  ; apply all valid actions on to the current state to get all possible successor states
  (append (next-state s 0 1) (next-state s 0 2)
          (next-state s 1 0) (next-state s 1 1)
          (next-state s 2 0)
  )
)

; Tests - all tests should output T
; (print "SUCC-FN TESTS")
; (print (equal (succ-fn '(1 1 t)) '((3 2 NIL) (3 3 NIL))))
; (print (equal (succ-fn '(3 3 t)) '((0 1 NIL) (0 2 NIL) (1 1 NIL))))
; (print (equal (succ-fn '(1 1 t)) '((3 2 NIL) (3 3 NIL))))
; (print (equal (succ-fn '(1 2 t)) '((2 2 NIL) (3 1 NIL) (3 2 NIL))))



; ON-PATH - checks whether the current state is on the stack of states visited by this depth-first search
; Arguments - s (current state), states (stack of states visited by MC-DFS)
; Returns - Boolean (t or NIL)

(defun on-path (s states)
  ; if states is NULL we have nothing to look at - return NIL
  (cond ((NULL states) NIL)
        ; if s == first element of "states", s is on the stack of states - return t
        ((equal s (first states)) t)
        ; else, recurse through the rest of "states" to check if s is in it
        (t (on-path s (rest states)))
  )
)

; Tests - all tests should output T
; (print "ON-PATH TESTS")
; (equal (on-path '(1 1 t) NIL) NIL)
; (print (equal (on-path '(1 1 t) '((0 1 NIL) (1 1 NIL) (0 2 NIL))) NIL))
; (print (equal (on-path '(0 1 NIL) '((0 1 NIL) (1 1 NIL) (0 2 NIL))) T))
; (print (equal (on-path '(1 1 t) '((1 1 t) (3 2 NIL) (3 3 NIL))) t))
; (print (equal (on-path '(0 2 t) '((0 1 NIL) (1 1 NIL) (0 2 NIL))) NIL))
; (print (equal (on-path '(0 2 NIL) '((0 1 NIL) (1 1 NIL) (0 2 NIL))) T))


; MULT-DFS - performs a depth-first search on each element of states in turn; serves as a helper function for MC-DFS
; Arguments - states (legal successor states from the current state), path (stack of states from the initial state to the current state)
; Returns - the path from the initial state to the goal state (if one exists); NIL otherwise

(defun mult-dfs (states path)
  ; there are no states to look through, return NIL
  (cond ((null states) NIL)
        ; if mc-dfs can find a path from the initial state to the final state, then return said path
        ((mc-dfs (first states) path) (mc-dfs (first states) path))
        ; else (if mc-dfs can not find a path), recurse using mult-dfs to hopefully try and find a path within the rest of the states
        (t (mult-dfs (rest states) path))
  )
)




; MC-DFS - performs a depth first search from a given state to the goal state
; Arguments - s (a state), path (path from the initial state to S)
; Returns - the path from the initial state to the goal state (if one exists); NIL otherwise

(defun mc-dfs (s path)
  ; if there are no possible sucessor states, return NIL
  (cond ((null (succ-fn s)) NIL)
        ; if s is in the path already, don't need to do anything - just return NIL
        ((on-path s path) NIL)
        ; if the final state has been reached, add it to the path and return it
        ((final-state s) (cons s path))
        ; else, recursively call mult-dfs on the list of the sucessor states
        (t (mult-dfs (succ-fn s) (cons s path)))
  )
)

; Tests - test should output T
; (print "MULT-DFS + MC-DFS TEST")
; (print (equal (mult-dfs '((2 2 NIL) (3 1 NIL) (3 2 NIL)) NIL) '((3 3 NIL) (0 2 T) (3 2 NIL) (0 3 T) (3 1 NIL) (2 2 T) (2 2 NIL))))
; (print (equal (mult-dfs '((3 3 NIL)) NIL) '((3 3 NIL))))
; (print (equal (mult-dfs '((1 1 NIL)) NIL) '((3 3 NIL) (0 2 T) (3 2 NIL) (0 3 T) (3 1 NIL) (2 2 T) (2 2 NIL) (3 1 T)(0 3 NIL) (3 2 T) (1 1 NIL))))
; (print (equal (mc-dfs '(3 3 T) NIL) '((3 3 NIL) (0 2 T) (3 2 NIL) (0 3 T) (3 1 NIL) (2 2 T) (2 2 NIL) (3 1 T) (0 3 NIL) (3 2 T) (0 2 NIL) (3 3 T))))