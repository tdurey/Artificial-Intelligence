(defun myreverse (lst)
    (if (null lst)
        nil
        (if (listp (car lst))
            (append (myreverse (cdr lst)) (list (myreverse (car lst))))
            (append (myreverse (cdr lst)) (list (car lst)))
        )
    )
)