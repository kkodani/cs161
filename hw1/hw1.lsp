;SUB-LIST: takes a list L, a number START, and a number LEN.
;Creates a new list whose head is L at position START, and is LEN long
(defun SUB-LIST (L START LEN)
		;find the element of L at position START
	(cond	((> START 0) (SUB-LIST (rest L) (- START 1) LEN))
		;make sure we aren't at the end of the list
		((null L) nil)
		;create a list using the next LEN elements of L
		((> LEN 0) (cons (first L) (SUB-LIST (rest L) START (- LEN 1))))
		(t nil)
	)
)

;SPLIT-LIST: takes a list L
;Creates a list of two lists, where the first list is the first half of L,
;and the second list is the second half of L.  If L is of odd length,
;the second list is longer.
(defun SPLIT-LIST (L)
		;if L has even length, create a list of two lists using SUB-LIST
	(cond	((evenp (length L)) (list (SUB-LIST L 0  (/ (length L) 2)) (SUB-LIST L (/ (length L) 2) (/ (length L) 2))))
		;if L has odd length, create a list of two lists using SUB-LIST, but make the first list smaller
		(t (list (SUB-LIST L 0  (/ (- (length L) 1) 2)) (SUB-LIST L (/ (- (length L) 1) 2) (/ (+ (length L) 1) 2))))
	)
)

;LIST2BTREE: takes a list LEAVES
;Turns LEAVES into a binary tree that fills up from right to left
(defun LIST2BTREE (LEAVES)
	(cond	((null LEAVES) nil)
		;if LEAVES has only one elt, return that elt
		((= (length LEAVES) 1) (first LEAVES))
		;if LEAVES has only two elts, return a list of both elts
		((= (length LEAVES) 2) LEAVES)
		;split LEAVES in half and apply LIST2BTREE on both halves
		(t (cons (LIST2BTREE (first (SPLIT-LIST LEAVES))) (cons (LIST2BTREE (second (SPLIT-LIST LEAVES))) nil)))
	)
)
;BTREE2LIST: takes a list TREE
;
(defun BTREE2LIST (TREE)
	(cond	((null TREE) nil)
		;if TREE is a atom, return a list of that atom
		((atom TREE) (list TREE))
		;create a list, breaking down the first elt of TREE, then calling BTREE2LIST
		;for the rest of TREE
		(t (append (BTREE2LIST (first TREE))  (BTREE2LIST(rest TREE))))
	)
)







