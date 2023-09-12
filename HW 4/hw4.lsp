;;;;;;;;;;;;;;
; Homework 4 ;
;;;;;;;;;;;;;;


; EXERCISE: Modify this function to decide satisifiability of delta.
; If delta is satisfiable, sat? returns a list of n integers that represents a model of delta,  
; otherwise it returns NIL. (See spec for details.)
; param n: number of variables in delta
; param delta: a CNF represented as a list of lists
(defun sat? (n delta)
  (bts '() n delta 1)
)


; Helper function that checks if a literal returns True for any of the literals in the assignment
; Arguments: x (an integer representing a literal in the CNF), a (list of assignments made so far)
; Returns: True if literal matches any assignment, and NIL otherwise
(defun literal? (x a)
  ; if there have been no assignments so far, return True
  (cond ((null a) t)
        ; if the first element of a == x, return True
        ((equal (abs x) (abs (car a))) (equal (first a) x))
        ; else, recursively call literal? on the rest of a
        (t (literal? x (cdr a)))
  )
)


; Helper function that checks if a literal within a clause returns True for the assignments
; Arguments: x (a clause - list of integers), a (list of assignments made so far)
; Returns: True if any of the literals work with the assignments, and NIL otherwise
(defun clause? (x a)
  ; if there is no clause, there's nothing to look at - return NIL
  (cond ((null x) NIL)
        ; recursively call literal? on each element in x, return True if any of the literals work with the assignments
        ((literal? (car x) a) t)
        ; else, recursively call clause? on the rest of x
        (t (clause? (rest x) a))
  )
)


; Helper function that checks if all of the clauses return T for the assignments
; Arguments: x (a CNF - list of clauses), a (list of assignments made so far)
; Returns: True if all of the clauses return T for the assignments, and NIL otherwise
(defun cnf? (x a)
  ; if there have been no assignments so far and cnf isn't null, return True
  (cond ((and (null a) (not (null x))) t)
        ; if there is no cnf, return True
        ((null x) t)
        ; else, recursively call clause? on the rest of x, and recursively call cnf? on the head of x, return True if both are true
        (t (and (cnf? (cdr x) a) (clause? (car x) a)))
  )
)


; Helper function that returns a list of assignments for the literals
; Arguments: idx (integer index of the literal), a (list of assignments made so far), val (boolean value that we want to assign to idx)
; Returns: a list of assignments for the literals
(defun assign (idx a val)
  ; if val == True, add the integer to the list
  (cond ((not (null val)) (append a (list idx)))
        ; if val == False, add the negated integer to the list
        (t (append a (list (- idx))))
  )
)


; Helper function that performs backtrack search
; Arguments: n (integer representing number of variables in the cnf), depth (integer representing depth), delta (a cnf), a (list of assignments)
; Returns: a list of assignments to the variables that would solve SAT, if there is a solution
(defun bts (a n delta depth)
  (cond 
        ; if the backtrack search reaches the maximum depth
        ((= (- depth 1) n)
            ; if the cnf is not true, return NIL
            (cond ((not (cnf? delta a)) NIL)
                  ; else (cnf is true), return the current assignments as they are correct
                  (t a)
            )
        )
        ; if the cnf can't possibly be true, return NIL
        ((not (cnf? delta a)) NIL)
        ; else, recursively call bts while assigning the next variable to both t/NIL, to see which one might work
        (t (or (bts (assign depth a t) n delta (+ 1 depth)) (bts (assign depth a NIL) n delta (+ 1 depth))))
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions that help you parse CNF from files in folder cnfs/
; You need not modify any functions in this section
; Usage (solve-cnf <path-to-file>)
; e.g., (solve-cnf "./cnfs/f1/sat_f1.cnf")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun split-line (line)
  (if (equal line :eof)
      :eof
      (with-input-from-string (s line) (loop for x = (read s nil) while x collect x))))

(defun read-cnf (filename)
  (with-open-file (in filename)
    (loop for line = (split-line (read-line in nil :eof)) until (equal line :eof)
      if (equal 'p (first line)) collect (third line)      ; var count
      if (integerp (first line)) collect (butlast line)))) ; clause

(defun parse-cnf (filename)
  (let ((cnf (read-cnf filename))) (list (car cnf) (cdr cnf))))

; Following is a helper function that combines parse-cnf and sat?
(defun solve-cnf (filename)
  (let ((cnf (parse-cnf filename))) (sat? (first cnf) (second cnf))))

