;Thane Durey

;match is used as a helper function
(defun match(term pattern)
	(match1 term pattern '(nil))
)

;match1 
(defun match1 (term pattern subst)
	(cond ((and (null term) (null pattern)) (removeNil subst))					;if both term and pattern are nil, then return the subst
		((or (null term) (null pattern)) nil)									;check if term or pattern are nil, then return nil
		((or (equal pattern 'u) (equal pattern 'v) (equal pattern 'w)					;check if the varible is u, v, w, x, y, or z and put this in dotted pairs with term
		     (equal pattern 'x) (equal pattern 'y) (equal pattern 'z)) (list (cons pattern term))) 
		((equal term pattern) (removeNil subst))											;check if term and pattern are equal, if so return the subst
		((or (atom term) (atom pattern)) nil)									;if the term or pattern are atoms, return nil
		(t (LET ((sigmacar (match (car term) (car pattern))))					;let sigmacar = (match (car term) (car pattern))
				(if (equal sigmacar nil)										;if sigmacar is equal to nil, return nil
					nil
					
					(match1 (applysub (union sigmacar subst) (cdr term)) (applysub (union sigmacar subst) (cdr pattern)) (union subst sigmacar))
				)													;we then return the match1 of the applysub and union sigmacar subst and the cdr of term.
																	;for the second argument for match1, we applysub and union sigmacar subst and the cdr of pattern
																	;the third agrument is the union subst with sugmacar
			)
		)
	)
)

;applysub applies substitution
(defun applysub (dotlst lst) ;
	(cond ((null lst) nil)
		((atom lst)(if (equal lst (caar dotlst)) (cdar dotlst) lst))		;checks if lst is equal to the caar of the dotlst, if so, it replaces it
		(t (cons (applysub dotlst (car lst))(applysub dotlst (cdr lst))))	;then apply a dotted pair with the car of the lst, and recursivly call applysub dotlst and cdr of lst
	)
)

;function to removes nil at beginning of the list
(defun removeNil (subst)
	(if (null (cdr subst))
		subst
		(cdr subst)
	)
)