(setq r1 '((+ x (- x)) 0))
(setq r2 '((+ 0 x) x))
(setq r3 '((+ x 0) x))
(setq r4 '((/ x x) 1))
(setq r5 '((* x 1) x))
(setq r6 '((* 1 x) x))
(setq r7 '((+ (- x) x) 0))
(setq r8 '((* 0 (- x)) 0))
(setq r9 '((/ 0 x) 0))
(setq r10 '((- (- x)) x))
(setq r11 '((- x x) 0))
(setq r12 '((/ (* x (+ y z)) (+ y z)) x))
(setq r (list r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12))
