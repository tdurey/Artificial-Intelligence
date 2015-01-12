
;This is the file DOIT.LSP - DO NOT CHANGE IT

(load 'tbsimplify)
(load 'r)
(setq term0 '(+ Mary (+ Sue (- Sue))))
(pprint (setq a0 (simplify term0 r)))
(setq term1 '(+ saw (+ (+ had (+ heard (- heard))) (- had))))
(pprint (setq a1 (simplify term1 r)))
(setq term2 '(+ (+ (- (* a b)) (* a b)) a))
(pprint (setq a2 (simplify term2 r)))
(setq term3 '(* big (/ little little)))
(pprint (setq a3 (simplify term3 r)))
(setq term4 '(+ (* 1 (/ 0 (+ ludicrous bright (* invisible dangerous)))) silly))
(pprint (setq a4 (simplify term4 r)))
(setq term5 '(+ (+ (- tempermental) tempermental) (- (- lovable))))
(pprint (setq a5 (simplify term5 r)))
(setq term6 '(+ (- ugly (* (/ big  big) ugly)) cute))
(pprint (setq a6 (simplify term6 r)))
(setq term7 '(+ (* (+ (- pig) pig)(- dog))(/ (* pig (+ dog cat))(+ dog cat))))
(pprint (setq a7 (simplify term7 r)))
(pprint (setq ans (list a0 a1 a2 a3 a4 a5 a6 a7)))

