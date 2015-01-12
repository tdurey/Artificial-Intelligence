; Thane Durey
; simplify.lsp
; Description - This program will take a set r of reductions and apply
;               them exhaustively to the term, returning the simplified version
;               of the term.  The set r of reductions is a list in which each 
;               member is a single reduction.  A single reduction is a list
;               Where the car is the left side and the cadr is the right side. 

; Simplify calles simplify1 and passes in the term, r, and r again to be used temporarily.
(defun simplify (term r)
    (cond ((or (or (null term)(null r)) (or (atom term)(atom r))) term)
        (t (simplify1 term r r))
    )
)

; After passing term into checkReductions, if we no longer have any changes with newTerm and term, we
; call the function simplify1 and pass in the newTerm, but this time checking all of our other rules.
; After we have checked through all our rules, we call finalCheck.
(defun simplify1 (term r subR)
    (cond ((null subR) (finalCheck term r r))
        (t 
            (setq newTerm(checkReductions term subR))
            (if (equal newTerm term)
                (simplify1 (checkReductions term (cdr subR)) r (cdr subR))
                (simplify newTerm r)
            )
        )
    )
)

; After going through our list of rules, we need to do a final check to see if any of our rules can 
; be applied again.  
(defun finalCheck (term r subR)
    (cond ((null subR) term)
        (t 
            (setq newTerm(checkReductions term subR))
            (if (equal newTerm term)
                (finalCheck (checkReductions term (cdr subR)) r (cdr subR))
                (simplify newTerm r)
            )
        )
    )
)

; searches for potential changes in our term, and checks with our rules.  If there
; are any changes, we call the function
(defun checkReductions (term r)
    (cond ((or (or (null term)(null r)) (or (atom term)(atom r))) term)
        (t
            (setq matchingPairs (match term (caar r)))
            (if (not (equal matchingPairs NIL))
                (replaceIteration matchingPairs r)
                (cons (checkReductions (car term) r) (checkReductions (cdr term) r))
            )
        )
    )
)

; replaceIteration will iterate through all our matchingPairs that we have, and 
; we send each matchingPair to replace to actually make these changes until all of 
; our matchingPairs have been added to our term
(defun replaceIteration (matchingPairs r)
    (cond ((null matchingPairs) r)
        ((atom r) (cdar matchingPairs))
        (t 
            (setq newR (cadr (myreplace (caar matchingPairs) (cdar matchingPairs) (car r))))
            (replaceIteration (cdr matchingPairs) newR)
        )
    )
)

; replaces any matches we have found with the new data
(defun myreplace (old new data)
    (cond ((null data) nil)
        ((atom data) (if (equal data old) new data))
        (t (cons (myreplace old new (car data)) (myreplace old new (cdr data))))
    )
)