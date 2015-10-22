;;;; multiplication
(defun mult (x y)
	(cond
		((= y 0) 0)
		(t (+ x (mult x (+ y -1))))))
;;;; mult wrapper
(defun multiply (x y)
	(if (< y 0)
		(setq res (negate y))
		(setq res y))
	(setq res (mult x res))
	(if (< y 0)
		(setq res (negate res)) res))
;;;; subtraction
(defun subtract (x y)
	(+ x (negate y))
)
;;;; absolute value
(defun abs-val (x)
	(cond
		((>= x 0) x)
		((< x 0) (negate x))))
;;;; negate
(defun negate (x &optional (neg 0))
	(cond
		((= x 0) neg)
		((> x 0) (negate (+ x -1) (+ neg -1)))
		(t (negate (+ x 1) (+ neg +1)))))
;;;; testing
;(defun test (x y)
;	(abs-val (+ x y)))