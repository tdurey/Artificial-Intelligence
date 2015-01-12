(defun lookup (x listPair)
	(if (null listPair)
		nil
		(if (eq x (caar listPair))
			(cadar listPair)
			(lookup x (cdr listPair))
		)
	)
)