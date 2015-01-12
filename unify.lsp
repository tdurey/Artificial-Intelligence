;Thane Durey

;unify is used as a helper function
(defun unify(term pattern)
	(unify1 term pattern '(nil))
)

;unify1 
(defun unify1 (term pattern subst)
	(cond ((and (null term) (null pattern)) subst)					;if both term and pattern are nil, then return the subst
		((or (null term) (null pattern)) nil)									;check if term or pattern are nil, then return nil
		((or (equal term 'u) (equal term 'v) (equal term 'w)					;check if the varible is u, v, w, x, y, or z and put this in dotted pairs with term
		     (equal term 'x) (equal term 'y) (equal term 'z)) (list (cons term pattern))) 
		((or (equal pattern 'u) (equal pattern 'v) (equal pattern 'w)					;check if the varible is u, v, w, x, y, or z and put this in dotted pairs with pattern
		     (equal pattern 'x) (equal pattern 'y) (equal pattern 'z)) (list (cons pattern term)))
		((equal term pattern) subst)											;check if term and pattern are equal, if so return the subst
		((or (atom term) (atom pattern)) nil)									;if the term or pattern are atoms, return nil
		(t (LET ((sigmacar (unify (car term) (car pattern))))					;let sigmacar = (unify (car term) (car pattern))
				(if (equal sigmacar nil)										;if sigmacar is equal to nil, return nil
					nil
					(if (equal subst '(nil))
						(unify1 (applysub (union sigmacar subst) (cdr term)) (applysub (union sigmacar subst) (cdr pattern)) sigmacar) ;if the substring is the literal list nil, then there are no changes that need to be made so far
						(unify1 (applysub (union sigmacar subst) (cdr term)) (applysub (union sigmacar subst) (cdr pattern)) (union sigmacar (applysub sigmacar subst))) ;if there is a literal list nil, just return the dotted pair
					)
				)													;we then return the unify1 of the applysub and union sigmacar subst and the cdr of term.
																	;for the second argument for unify1, we applysub and union sigmacar subst and the cdr of pattern
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