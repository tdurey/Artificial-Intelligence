(defun lat?(x)
	(if (null x)
		T
		(if (and (listp x)(atom(car x) ))
			(lat? (cdr x))
			nil
		)
	)
)