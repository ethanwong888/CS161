; Ethan Wong
; UID 305319001
; DIS 1C

; #1
; TREE-CONTAINS, which takes two arguments N and TREE, and checks whether number N appears in the ordered tree TREE
; Arguments: N (a number), TREE (a tree)
; Returns: Boolean (t or NIL)
; OK

(defun TREE-CONTAINS(N TREE)
  ; if the tree is just a number, return whether N == TREE
  (cond ((numberp TREE) (equal N TREE))
        ; if m = N, then return True
        ((equal (second TREE) N) t)
        ; else, recurse on R (THIRD TREE) and L (FIRST TREE) to see if N is in either of those ordered trees
        (t (or (TREE-CONTAINS N (THIRD TREE)) (TREE-CONTAINS N (FIRST TREE))))
  )
)


; #2
; TREE-MAX, which takes one argument TREE, and returns the maximum number appearing in the ordered tree TREE
; Arguments: Tree (a tree)
; Returns: Number (max number found in the tree)
; OK

(defun TREE-MAX(TREE)
  ; if the tree is just a number, return that number
  (cond ((numberp TREE) TREE)
        ; recurse to the right-most number in R, as the MAXNUMBER should be the largest value in the right ordered tree
        (t (TREE-MAX (THIRD TREE)))
  )
)


; #3
; TREE-ORDER, which takes one argument TREE, and returns a post-ordered list of the numbers appearing in the ordered tree TREE
; Arguments: Tree (a tree)
; Returns: list (post-ordered elements of Tree)
; OK

(defun TREE-ORDER(TREE)
  ; if the tree is just a number, it's already post-ordered -- just return it as a list
  (cond ((numberp TREE) (list TREE))
        ; else recursively order L, R, m, then append them together
        (t (append (TREE-ORDER(FIRST TREE)) (TREE-ORDER(THIRD TREE)) (TREE-ORDER(SECOND TREE))))
  )
)


; #4
; SUB-LIST, that takes a list L and two non-negative integers START and LEN, and returns the sub-list of L starting at position START and having length LEN
; Arguments: L (a list), START (a number) LEN (a number)
; Returns: a list
; OK

(defun SUB-LIST(L START LEN)
  ; if list is null return NIL
  (cond ((null L) NIL)
        ; if len is 0, then the sub-list is length 0 and we return the empty list
        ((equal LEN 0) NIL)
        ; if start == 0, combine head with tail, while recursively calling SUB-LIST on tail to get the rest of the sub-list
          ; decrement LEN because we already took "FIRST L", which is an element of the sub-list
        ((equal START 0) (cons (FIRST L) (SUB-LIST (REST L) 0 (- LEN 1))))
        ; if start > 0, recursively call SUB-LIST on tail (adjusting start point to compensate losing the head of L)
        ((> start 0) (SUB-LIST (REST L) (- START 1) LEN))
  )
)


; #5
; SPLIT-LIST, that takes a list L, and returns a list of two lists L1 and L2, in that order
; Arguments: L (a list)
; Returns: a list
; OK

(defun SPLIT-LIST (L)
	; L1LEN represents  len(L1)
	; if len(L) is even, just divide len(L) by 2
	; if len(L) is odd, add one to len(L) and divide by 2
  (let ((L1LEN (cond ((evenp (length L)) (/ (length L) 2))
		                 ((oddp (length L)) (/ (+ (length L) 1) 2))))

		  ; L2LEN represents len(L2)
			; if len(L) is even, just divide len(L) by 2
			; if len(L) is odd, subtract one from len(L) and divide by 2
			(L2LEN (cond ((evenp (length L)) (/ (length L) 2))
												((oddp (length L)) (/ (- (length L) 1) 2)))))

	; use SUB-LIST to split L now that we know where L1 and L2 start and end at
	; return answer as a list that contains L1 and L2 (L1, L2)
	(list (SUB-LIST L 0 L1LEN) (SUB-LIST L L1LEN L2LEN))))


; #6
; BTREE-HEIGHT, which takes a binary tree TREE, and returns the height of TREE
; Arguments: TREE (a tree)
; Returns: a number
; OK

(defun BTREE-HEIGHT(TREE)
  ; if TREE is just a single number, then its height is 0
  (cond ((numberp TREE) 0)
        ; else we recurse down to the next level (think of normal tree traversal)
        (t (let* ((R (+ 1 (BTREE-HEIGHT (second TREE))))
                  (L (+ 1 (BTREE-HEIGHT (first TREE)))))

          ; if right branch is deeper than left branch, return right branch
          ; else (left branch is deeper than right branch), return left branch
		      (if (> R L) R L))
        )
  )
)


; #7
; LIST2BTREE, that takes a non-empty list of atoms LEAVES, and returns a binary tree
; Arguments: LEAVES (a list)
; Returns: binary tree (as a list)
; OK

(defun LIST2BTREE (LEAVES)
  ; len(LEAVES) == 1, so the binary tree is just that single element in LEAVES
  (cond ((= (length LEAVES) 1) (first LEAVES))
        ; else
        (t
          ; split LEAVES in two halves, new variable is 2LEAF
          ; R-2LEAF is the second half of 2LEAF (the right half of LEAVES)
          ; L-2LEAF is the first half of 2LEAF (the left half of LEAVES)
          (let* ((2LEAF (SPLIT-LIST LEAVES)) (R-2LEAF (second 2LEAF)) (L-2LEAF (first 2LEAF)))
            ; recurse on R-TREE to build right side of binary tree
            ; recurse on L-TREE to build left side of binary tree
            (let ((R-TREE (LIST2BTREE R-2LEAF)) (L-TREE (LIST2BTREE L-2LEAF)))
                  ; combine the left and right sides of the binary tree, return
                  (list L-TREE R-TREE))
          )
        )
  )
)


; #8
; BTREE2LIST, that takes a binary tree TREE as input, and returns a list of atoms
; Arguments: TREE (a binary tree)
; Returns: a list
; OK

(defun BTREE2LIST(TREE)
  ; if TREE only has a single element, return that element (as a list)
  (cond ((numberp TREE) (list TREE))
        ; else (TREE has more than one element), just recurse on first and second, append together
        (t (append (BTREE2LIST (first TREE)) (BTREE2LIST (second TREE))))
  )
)


; #9
; IS-SAME, that takes two LISP expressions E1 and E2 whose atoms are all numbers, and checks whether the expressions are identical
; Arguments: E1, E2 (both LISP expressions)
; Returns: Boolean (t or NIL)
; OK

(defun IS-SAME(E1 E2)
  ; if E1 and E2 are null, then they are technically equivalent so return True
  (cond ((and (null E1) (null E2)) t)
        ; if E1 and E2 are both numbers, return whether they are equal
        ((and (numberp E1) (numberp E2)) (= E1 E2))
        ; if E1 and E2 are both lists, check if they have the same head and the same tail. if they both match, the lists are the same
        ((and (listp E1) (listp E2)) (and (IS-SAME (car E1) (car E2)) (IS-SAME (cdr E1) (cdr E2))))
        ; else (E1 and E2 are not of the same type - Null, Number, or List) so they can not be equivalent
        (t NIL)
  )
)


; #10
; FLATTEN-APPEND, that takes two LISP expressions E1 and E2 and appends all the atoms of E2 to E1,
  ; in left-to-right, depth-first order of occurrence of atoms in E2
; Arguments: E1, E2 (both LISP expressions)
; Returns: a list
; OK

(defun FLATTEN-APPEND(E1 E2)
  ; if E2 is empty, just return E1 because there is nothing to append
  (cond ((null E2) E1)
        ; if E2 is just a number, simply append E2 (as a list) to E1
        ((numberp E2) (append E1 (list E2)))
        ; else (E2 is a non-empty list), recurse on E1 and E2
          ; first want to recurse on E1 with head of E2, then take that result and recurse with the rest of E2
        (t (FLATTEN-APPEND (FLATTEN-APPEND E1 (first E2)) (rest E2)))
  )
)
