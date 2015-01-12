; Thane Durey
; ddb - Simple Deductive Data Base

(defun ? (term)
	(cond 	
	
		; If we have 2 terms in our input, that means we are comparing
		; two things (i.e. (mortal fido) is fido a mortal), or we are 
		; requesting everyone/everything that is this type (i.e. (mammal z) is 
		; wanting everything that is a mammal which expects (FIDO LASSIE SOCRATES
		; PLATO FELIX LEO) as the answer since these are all the mammals in our
		; database)
		((equal 2 (length term))
			(cond
				((is-var (cadr term))
					(getAll term term db () ())
				)
				(T
					(yesOrNo term term db ())
				)
			)
		)
		
		; If we have 3 terms in our input, this means we are checking an action
		; of how one thing feels towards another.  If our variable is in the middle
		; of our 3 arguments, then we are wanted to get all of the things that match 
		; this.  If our variable is on the far right side of our 3 arguments, then we
		; are wanting to see, for example (hates fido y), if fido hates anything/everything.
		; This will return a Yes or a No.
		((equal 3 (length term))
			(cond
				((is-var (caddr term))
					(yesOrNo term term db ())
				)
				((is-var (cadr term))
					(getAll term term db () ())
				)
				(T
					(yesOrNo term term db ())
				)
			)
		)
	)
)

; If we are trying to prove that something has an is-a relationship,
; then we need to check in our database right to left.  For example, if
; we our input was (mortal fido), we need to check on the right side of
; our database and see if we can mach (mortal fido) with anything.  When we 
; come down our database, we use unify to make a match.  When we come to 
; ((dog x4)(mortal x4)), we see that (mortal fido) matches (motal x4), we use unify
; and then we recursively call our function again to check for our new term (dog fido).
; When we check our database again, and we find an exact match, and we read that the
; car of this list is T, we output Yes.  Otherwise, we continue in our rules, once we have 
; reached the end of the database(db), we output No, because we have not found a match.
(defun yesOrNo (originalTerm term remainingDB incorrectList)
	(cond
		; If we have nothing in our term, then we have reached the end of the database(db)
		; and we have proven that this is not True
		((null remainingDB)
			(if(equal originalTerm term)
				'No
				(yesOrNo originalTerm originalTerm db (append incorrectList (list term)))
			)
		)
		; If the car of the list is T, then we have proven that this is True
		((and (equal (unify term (cadar remainingDB)) '(nil)) (equal (caar remainingDB) T))
			'Yes
		)
		(T
			; If our unify with our term and the cdr of the the first list in the
			; database(db) is nil, that means it is not a match and we keep going
			; through our database(db) and check to see for more matches
			(setq newLeftQuery (applysub (unify term (cadar remainingDB)) (caar remainingDB)))
			(if (equal (unify term (cadar remainingDB)) nil)
				(yesOrNo originalTerm term (cdr remainingDB) incorrectList)
				(if (isInList newLeftQuery incorrectList)
					(yesOrNo originalTerm term (cdr remainingDB) incorrectList)
					(yesOrNo originalTerm newLeftQuery db incorrectList)
				)
			)
		)
	)
)

; This function checks our entire list of incorrectList
(defun isInList (newLeftQuery incorrectList)
	(if (null incorrectList)
		nil
		(if(equal (car incorrectList) newLeftQuery)
			T
			(isInList newLeftQuery (cdr incorrectList))
		)
	)
)

; This function will retrieve all of the matches that matches the term 
; we have inputed.  We must check our entire database(db) thoroughly to 
; find all the matches that apply.
(defun getAll (originalTerm term remainingDB incorrectList correctList)
	(cond
		((and (equal (caar remainingDB) t) (not (equal (unify term (cadar remainingDB)) nil)))
			(if (equal (isInList (car (cdadar remainingDB)) correctList) T)
				(getAll originalTerm term (cdr remainingDB) incorrectList correctList)
				(getAll originalTerm originalTerm db incorrectList (append correctList (list (car (cdadar remainingDB)))))
			)
		)
		((and (null remainingDB) (equal term originalTerm))
			correctList
		)
		((null remainingDB) 
			(getAll originalTerm originalTerm db (append incorrectList (list term)) correctList)
		)
		((and (not(is-var (cadr (unify term (cadar remainingDB))))) (equal (yesOrNo (applysub (unify term (cadar remainingDB)) (caar remainingDB)) (applysub (unify term (cadar remainingDB)) (caar remainingDB)) db ()) 'Yes))
			(append correctList (list(cdadr (unify term (cadar remainingDB)))))
			
		)
		(T
			(setq newQuery (applysub (unify term (cadar remainingDB)) (caar remainingDB)))
			(if (equal (unify term (cadar remainingDB)) nil)
				(getAll originalTerm term (cdr remainingDB) incorrectList correctList)
				(if (equal (isInList newQuery incorrectList) T)
					(getAll originalTerm term (cdr remainingDB) incorrectList correctList)
					(getAll originalTerm newQuery db incorrectList correctList)
				)
			)
		)
	)
)