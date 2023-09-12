; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.

; For reloading modified code. 
(defun reload()
  (load "hw3.lsp")
)

; For loading a-star.lsp.
(defun load-a-star()
  (load "a-star.lsp")
)


; Reloads hw3.lsp and a-star.lsp
(defun reload-all()
  (reload)
  (load-a-star)
)


; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
)

(defun isWall (v)
  (= v wall)
)

(defun isBox (v)
  (= v box)
)

(defun isKeeper (v)
  (= v keeper)
)

(defun isStar (v)
  (= v star)
)

(defun isBoxStar (v)
  (= v boxstar)
)

(defun isKeeperStar (v)
  (= v keeperstar)
)


; Helper function of getKeeperPosition

(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
)


; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).

; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.

(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
)


; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).

(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
);end 

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of the game.
; (neither any boxes nor the keeper is on a non-goal square)

; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.

; We are at a final state when all the boxes + the keeper are standing on top of a goal.
; Thus, we shouldn't see the symbol for Keeper (3, @) or the symbol for Box (2, $). 
; We should see the symbol for Keeper + Goal (6, +), or the symbol for Box + Goal (5, *)

; Arguments: s (a state of the game)
; Returns: Boolean (t, nil) - if s is a goal state of the game

(defun goal-test (s)
  ; if the state is empty, treat it as a goal state - return t
  (if (null s) t
    ; else if the state is not empty:
      ; if there is a Box or Keeper in the first row, return NIL (we shouldn't see Box or Keeper at all if it is a goal state)
      ; else, check the rest of the rows to see if we find a Box or Keeper
    (if (or (position box (first s)) (position keeper (first s))) NIL (goal-test (rest s)))
  )
)


; EXERCISE: Modify this function to return the list of 
; sucessor states of s.

; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.

; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))

; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.


; Returns (integer representation) of the object located at (r, c) in the matrix
(defun getRC (r c)
  ; if r is null (bad row value) or c is null (bad column value), return wall - there is nothing to look at here
	(cond ((or (null r) (null c)) 1)
        ; if c < 0 or c > (len(r) - 1) (column value is past the end of r), return wall - there is nothing to look at here
        ((or (< c 0) (> c (- (length r) 1))) 1)
        ; if c = 0, return the the first object in r (r, 0)
        ((= 0 c) (first r))
        ; else, recursively call getRC on the rest of the row while decrementing c
        (t (getRC (rest r) (- c 1))) 
	)
)


; Returns a newly created row with object at (r, c) set to val
(defun RC-to-VAL (r c val)
  ; if r is null (bad row value) or c is null (bad column value), return NIL
	(cond ((or (null r) (null c)) NIL)
        ; if c is 0, prepend val to (rest r) - this means val is just replaces the first element of r
        ((= 0 c) (cons val (rest r)))
        ; else, keep the first element of r (since we know we aren't replacing that one now) and append it to the result of recursviely calling RC-to-VAL on the rest of r
        (t (cons (first r) (RC-to-VAL (rest r) (- c 1) val)))
	)
)


; Returns the (integer representation) of the object located at (r, c) in state s
(defun get-square (s r c)
  ; if r is null (bad row value) or c is null (bad column value), return wall - there is nothing to look at here
	(cond ((or (null r) (null c)) 1)
        ; if s is null (bad state), return wall - there is nothing to look at here
        ((null s) 1)
        ; if r < 0 or r > (len(s) - 1) (row value is past the end of s), return wall - there is nothing to look at here
        ((or (< r 0) (> r (- (length s) 1))) 1)
        ; if r = 0, look through the row to get object at column
        ((= 0 r) (getRC (first s) c))
        ; else, recursively call get-square on the rest of the state while decrementing r
        (t (get-square (rest s) (- r 1) c))
	)
)


; Returns a new state S'. S' is the same as s, but the object at (r, c) is replaced with val
(defun set-square (s r c val)
  ; if r is null (bad row value) or c is null (bad column value), return NIL
	(cond ((or (null r) (null c)) NIL)
        ; if we are changing an element in the first row, change the element and prepend the newly created row to the rest of s
        ((= 0 r) (cons (RC-to-VAL (first s) c val) (rest s)))
        ; else, keep the first row of s (since we know we aren't replacing that one now) and append it to the result of recursviely calling set-square on the rest of s
		    (t (cons (car s) (set-square (cdr s) (- r 1) c val)))
	)
)


(defun try-move (s r c d)
  ; the new (hypothetical) position of the keeper after moving (DOWN, RIGHT, UP, OR LEFT)
	(let* ((keeperRC (cond ((equal d 'U) (list (- r 1) c))
                         ((equal d 'D) (list (+ r 1) c))
                         ((equal d 'L) (list r (- c 1)))
                         ((equal d 'R) (list r (+ c 1)))     
				          )
			   )
         ; column that the keeper is (hypothetically) in after moving
         (keeperC (second keeperRC))
         ; integer representation of the object that is at (r, c)
         (og-object (get-square s r c))
         ; row that the keeper is (hypothetically) in after moving
         (keeperR (first keeperRC))
         ; integer representation of the object that is at (keeperR, keeperC)
         (new-object (get-square s keeperR keeperC))
        )
    
		(cond 
      ; if new position is a box-star or box
      ((or (isBoxStar new-object) (isBox new-object))
        ; the new position of the box after keeper pushes it (DOWN, RIGHT, UP, OR LEFT)
        (let* ((pushedBoxRC (cond ((equal d 'U) (list (- keeperR 1) keeperC))
                                  ((equal d 'D) (list (+ keeperR 1) keeperC))
                                  ((equal d 'L) (list keeperR (- keeperC 1)))
                                  ((equal d 'R) (list keeperR (+ keeperC 1)))        
                            )
                )
              ; column that the box is in after being pushed
              (pushedBoxC (second pushedBoxRC))
              ; row that the box is in after being pushed
              (pushedBoxR (first pushedBoxRC))
              ; the (integer representation) of the object located at (pushedBoxR, pushedBoxC) in state s
              (pushedBox-object (get-square s pushedBoxR pushedBoxC))
              )

          (cond
            ; if pushing the box onto a goal
            ((isStar pushedBox-object)
              (cond
                ; if the box was originally sitting on a blank before being pushed
                ((isBox new-object) 
                  ; temp is the state we get after pushing the box into its new position (a blank space)
                  (let* ((temp (set-square s pushedBoxR pushedBoxC boxstar))
                        ; keeper-pushedBox is the state we get after pushing the box into its new position, and the keeper is now on a blank space
                        (keeper-pushedBox (set-square temp keeperR keeperC keeper)))
                    ; if keeper moved off of a goal, mark it back to just being a goal
                    (cond ((isKeeperStar og-object) (set-square keeper-pushedBox r c 4))
                          ; if keeper moved off of a blank spot, mark it back to just being a blank spot
                          ((isKeeper og-object) (set-square keeper-pushedBox r c 0))
                    )
                  )
                )

                ; if the box was originally sitting on a goal before being pushed
                ((isBoxStar new-object)
                  ; temp is the state we get after pushing the box into its new position (a goal)
                  (let* ((temp (set-square s pushedBoxR pushedBoxC boxstar))
                        ; keeper-pushedBox is the state we get after pushing the box into its new position, and the keeper is now on a goal
                        (keeper-pushedBox (set-square temp keeperR keeperC keeperstar)))
                    ; if keeper moved off of a goal, mark it back to just being a goal
                    (cond ((isKeeperStar og-object) (set-square keeper-pushedBox r c 4))
                          ; if keeper moved off of a blank spot, mark it back to just being a blank spot
                          ((isKeeper og-object) (set-square keeper-pushedBox r c 0))
                    )
                  )
                )
              )
            )

            ; ILLEGAL: pushing the box into a space where another box already resides, or pushing the box into a wall
            ((or (isBoxStar pushedBox-object) (isBox pushedBox-object) (isWall pushedBox-object)) NIL)

            ; if pushing the box onto a blank space
            ((isBlank pushedBox-object)
              (cond 
                ; if the box was originally sitting on a blank before being pushed
                ((isBox new-object) 
                  ; temp is the state we get after pushing the box into its new position (a blank space)
                  (let* ((temp (set-square s pushedBoxR pushedBoxC box))
                        ; keeper-pushedBox is the state we get after pushing the box into its new position, and the keeper is now on a blank space
                        (keeper-pushedBox (set-square temp keeperR keeperC keeper)))
                    ; if keeper moved off of a goal, mark it back to just being a goal
                    (cond ((isKeeperStar og-object) (set-square keeper-pushedBox r c 4))
                          ; if keeper moved off of a blank spot, mark it back to just being a blank spot
                          ((isKeeper og-object) (set-square keeper-pushedBox r c 0))
                    )
                  )
                )

                ; if the box was originally sitting on a goal before being pushed
                ((isBoxStar new-object)
                  ; temp is the state we get after pushing the box into its new position (a goal)
                  (let* ((temp (set-square s pushedBoxR pushedBoxC box))
                          ; keeper-pushedBox is the state we get after pushing the box into its new position, and the keeper is now on a goal
                        (keeper-pushedBox (set-square temp keeperR keeperC keeperstar)))
                    ; if keeper moved off of a goal, mark it back to just being a goal
                    (cond ((isKeeperStar og-object) (set-square keeper-pushedBox r c 4))
                          ; if keeper moved off of a blank spot, mark it back to just being a blank spot
                          ((isKeeper og-object) (set-square keeper-pushedBox r c 0))
                    )
                  )
                )    
              )
            )
          )
        )
      )

      ; if new position is a goal
      ((isStar new-object) 
        ; set keeper-pushedBox as the new state where the keeper has moved to the goal
        (let ((keeper-pushedBox (set-square s keeperR keeperC 6)))
          ; if keeper moved off of a goal, mark it back to just being a goal
          (cond ((isKeeperStar og-object) (set-square keeper-pushedBox r c 4))
                  ; if keeper moved off of a blank spot, mark it back to just being a blank spot
                ((isKeeper og-object) (set-square keeper-pushedBox r c 0))
          )
        )
      )
      
      ; if new position is a wall, return NIL - keeper can't walk into a wall
      ((isWall new-object) NIL)

      ; if new position is a blank spot
      ((isBlank new-object)
        ; set keeper-pushedBox as the new state where the keeper has moved to the blank spot
        (let ((keeper-pushedBox (set-square s keeperR keeperC 3)))
          ; if keeper moved off of a goal, mark it back to just being a goal
          (cond ((isKeeperStar og-object) (set-square keeper-pushedBox r c 4))
                  ; if keeper moved off of a blank spot, mark it back to just being a blank spot
                ((isKeeper og-object) (set-square keeper-pushedBox r c 0))
          )
        )
      )
		)
	)
)


(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
         (x (first pos))
         (y (second pos))
	       (result (list (try-move s y x 'U) (try-move s y x 'D) (try-move s y x 'L) (try-move s y x 'R)))
        )
    (cleanUpList result)
  )
)


; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.

; Always just returns 0.
(defun h0 (s)
  0
)

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.

; This heuristic is admissible because it will never overestimate the number of steps that we need to take from s to reach the goal state.
; We can only move one block per move, so the number of moves needed to reach the final state is at greater than or equal to the number of boxes that still have to be moved to goal states.
; Logically, this means that the heuristic is incapable of overestimating the cost needed to reach the final state.

(defun h1 (s)
  ; if the state is empty, there can not be any boxes to be misplaced - return 0
  (if (null s) 0 
      ; else (state is not empty), find how many misplaced boxes are in first row and recurse on the rest of the rows, then add together
      (+ (count box (first s)) (h1 (rest s)))   
  )
)


; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 

; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.

; Finds the Manhattan Distance between Point 1 (a, b) and Point 2 (c, d)
; Manhattan distance is calculated as |y1-y2| + |x1-x2|
(defun manhattan (a b c d)
  (+ (abs (- c d)) (abs (- a b)))
)

; Returns the shortest Manhattan Distance between an object and the goals
(defun shortest-manhattan (object goals)
  ; dist is the Manhattan Distance between an item and goal
	(let* ((dist (manhattan (first object) (first (first goals)) (second object) (second (first goals)))))
    ; if there's only one goal, return dist
		(cond ((= 1 (length goals)) dist)
			    ; else (there are multiple goals), return the minimum between dist and the dists of the object with rest of the goals
			    (t (min (shortest-manhattan object (rest goals)) dist))
		)
	)
)

; Returns the sum of all the distances between box and goal for the boxes that are not currently on a goal
(defun all-dist (object goals)
  ; if there are no objects, there can't be any distance from object to goal
  (cond ((null object) 0)
        ; else (there are objects), recursively sum the minimum distance between the first object and goal with the minmium distance of the rest of the objects and goal
        (t (+ (shortest-manhattan (first object) goals) (all-dist (rest object) goals)))
  )
)

; Returns a list of coordinates showing the coordinates of a certain type of element in a specific row
(defun type-row (Type Row R C)
  ; if Row is empty, we won't find any objects of any type in it - return NIL
  (cond ((null Row) NIL)
        ; if the current object is of type 'Type', then we add its coordinates to the list, and continue recursing through the rest of the row
        ((equal (first Row) Type) (cons (list R C) (type-row Type (rest row) R (+ 1 C))))
        ; else (the current object is NOT of type 'Type') then we just continue recursing through the rest of the row
        (t (type-row Type (rest row) R (+ C 1)))
  )
)

; Returns a list of coordinates showing the coordinates of a certain type of element in the entire game board
(defun type-board (Type Matrix R C)
  ; if Matrix is empty, we won't find any objects of any type in it - return NIL
  (cond ((null Matrix) NIL)
        ; else (Matrix is not empty), use type-row to look through all the rows one-by-one, recursively and add the relevant coordinates to the final list
        (t (append (type-row Type (first Matrix) R C) (type-board Type (rest Matrix) (+ 1 R) C)))
  )
)


(defun h2 (s)   
	(let* 
    (
      ; list of coordinates of all the goals
      (goalCoord (type-board star s 0 0))
      ; list of coordinates of all the boxes
      (boxCoord (type-board box s 0 0))
      ; position of the keeper
      (keeperRC (getKeeperPosition s 0))
      ; sum of all the distances between box and goal for the boxes that are not currently on a goal - this value should be 0 when we have solved the puzzle
      (sum-dist (all-dist boxCoord goalCoord))
    )

    
		(cond   
      ; if all boxes are on goals, just have to move keeper to a goal
      ((equal 0 sum-dist)
        ; if there are boxes, return minimum distance between keeper and goal
        (cond ((not (null boxCoord)) (shortest-manhattan keeperRC goalCoord))
              ; else (there are no boxes), return 0
              (t 0)
        )
      )

      ; if "isKeeperStar" returns t, then keeper is in goal position and game is finished - return 0
      ((isKeeperStar (get-square s (second keeperRC) (first keeperRC))) 0)
      
      ; else (if there are boxes still not on goals)
      (t
        ; if there are no more boxes, add sum-dist to the minimum distance between the keeper and the closest box
        (cond ((not (null boxCoord)) (+ sum-dist (shortest-manhattan keeperRC boxCoord)))
              ; else (there are no boxes), return the sum of all the distances between box and goal for the boxes that are not currently on a goal
              (t sum-dist)
        )
      )     
		)
	)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also provide a number which indicates the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below has optimal solution depth 6.
 | As for the solution depth, any admissible heuristic must make A* return an optimal solution. So, the depths of the optimal solutions provided could be used for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#
;(6)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)))

;(15)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 4 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(13)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 4 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(17)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 4 0)
	   (0 3 1 0 0 0 0)))

;(12)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 4 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(13)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 4 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(47)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 4 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 4 0 4 1)
	   (1 1 1 1 1 1)))

;(34)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 4 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(59)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 4 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(?)
(setq p11 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 4 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 4 0)
	    (0 0 1 4 0 0 0)))

;(?)
(setq p12 '((1 1 1 1 1 0 0 0)
	    (1 0 0 4 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(?)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 4 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(?)
(setq p14 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))

;(?)
(setq p15 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 4 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
