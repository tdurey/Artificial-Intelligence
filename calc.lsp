;strictly assuming there will only be problems like the ones given to us
(defun calc(lst)
	(if (listp (car lst))
		(calc (append (list (calc (car lst))) (cdr lst)))
		(if (listp (car (last lst)))
			(calc (append (butlast lst) (list (calc (car (last lst))))))
			(if (equal (cadr lst) '+)
				(+ (car lst) (car (last lst)))
				(if (equal (cadr lst) '-)
					(- (car lst) (car (last lst)))
					(if (equal (cadr lst) '*)
						(* (car lst) (car (last lst)))
						(/ (car lst) (car (last lst)))
					)
				)
			)
		)
	)
)