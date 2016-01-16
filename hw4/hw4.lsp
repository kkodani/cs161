;queens
;top level function
;the first function eliminates unecessary parantheses
(defun QUEENS (n)
	(first (next-state nil n 1 1))
)

;next-state
;checks to see if we have reached a goal state
;calls place-queen with a list of legal moves
(defun next-state (s n row col)
	(cond	;((= (length s) n) (cons s nil))
		((> row n) (cons s nil))
		(t (place-queen s n row col (legal-col s n row col)))
	)
)

;place-queen
;does a dfs by placing a queen at each legal move and exploring the next-state from each play
(defun place-queen (s n row col legalMoves)
	(cond	((null legalMoves) nil)
		((cond	((null (next-state (append s (list (first legalMoves))) n (+ row 1) 1)) (place-queen s n row col (rest legalMoves)))
			(t (next-state (append s (list (first legalMoves))) n (+ row 1) 1))
		))
	)
)

;legal-col
;returns a list of legal moves
(defun legal-col (s n row col)
	(cond	((> col n) nil)
		((null (conflicts s n row col)) (append (list col) (legal-col s n row (+ col 1))))
		(t (legal-col s n row (+ col 1)))
	)
)

;conflicts
;checks for vertical conflicts and diagonal conflicts by calling the respective functions
;note that horizontal conflicts can never occur due to placing order
(defun conflicts (s n row col)
	(or (col-conflict s col) (diag-conflict s n row col))
)

;col-conflict
;checks list of already placed queens to see if column is already occupied
(defun col-conflict (s col)
	(cond	((null s) nil)
		((= (first s) col) t)
		(t (col-conflict (rest s) col))
	)
)

;diag-conflict
;checks diagonal conflict by calling two functions that explore different diagonals
(defun diag-conflict (s n row col)
	(cond	((null s) nil)
		((or (NW s n row col) (NE s n row col)) t)
		(t nil)
	)
)

;NW
;checks for northwest diagonal conflicts
;not that diagonal conflicts in a southern direction can never occur due to placing order
(defun NW (s n row col)
	(cond	((or (= row 0) (= col 0)) nil)
		((not (null (first (nthcdr (- row 1) s))))
                        (cond   ((= (first (nthcdr (- row 1) s)) col) t)
                        	(t (NW s n (- row 1) (- col 1)))
			)
                )
		(t (NW s n (- row 1) (- col 1)))
	)
)

;NE
;checks for northeast diagonal conflicts
(defun NE (s n row col)
        (cond   ((or (= row 0) (> col n)) nil)
                ((not (null (first (nthcdr (- row 1) s))))
			(cond	((= (first (nthcdr (- row 1) s)) col) t)
				(t (NE s n (- row 1) (+ col 1)))
			)
		)
                (t (NE s n (- row 1) (+ col 1)))
        )
)

