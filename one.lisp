;;;; Transform Roman Numeral to Decimal
(defun romanToDecimal (l)
	(setq revl (reverse l))
	;;(print revl) ; testing
	(if (eq (rom-check l 0 0 0 0) 0)
		(r2d-recurse revl)
		(print "error"))))
;;;; Check Roman Numeral against Rules
;;;; error instantiated to 0, set to 1 on error
(defun rom-check (l error one-prev two-prev tre-prev)
	(setq digr (car l))
	;; set roman numeral relative value
	(cond
		((eq digr 'I) (setq digd 1))
		((eq digr 'V) (setq digd 2))
		((eq digr 'X) (setq digd 3))
		((eq digr 'L) (setq digd 4))
		((eq digr 'C) (setq digd 5))
		((eq digr 'D) (setq digd 6))
		((eq digr 'M) (setq digd 7))
		(t (setq digd Nil)))
	;; ascending order
	;; subtractions -- one symbol, subtraction symbol size, powers of 10 
	(cond 
		((eq digd Nil) 
			(cond 
				((eq l Nil) )
				(t (setq error 1))))
		(t
			(cond
				((and (< digd 7) (eq digd one-prev) (eq digd two-prev) (eq digd tre-prev)) (setq error 1)) ; more than three of same symbol
				((and (eq digd one-prev) (eq digd two-prev) (or (= digd 2) (= digd 4))) (setq error 1)) ; more than two of V or L
				((and (> one-prev 0) (> two-prev 0) (> digd one-prev) (> digd two-prev)) (setq error 1)) ; more than one lesser symbol in subtraction
				((and (> one-prev 0) (> digd one-prev) (> (- digd one-prev) 2)) (setq error 1) ; subtraction by too small a symbol
				((and (> digd one-prev) (or (= one-prev 2) (= one-prev 4) (= one-prev 6))) (setq error 1))) ; subtraction by not power of 10
			)))
;	(print error)
	(cond
		((eq digd Nil) error)
		((eq error 1) error)
		(t (rom-check (cdr l) error digd one-prev two-prev))))
;;;; Calculate Decimal Value from Roman Numeral
;;;; valr: current roman numeral digit
;;;; vald: current decimal digit
;;;; last-val: previous decimal digit or Nil
(defun r2d-recurse (l &optional last-val)
	(setq valr (car l))
	;;(print valr) ; testing
	;; Set Value of current Roman Numeral Atom
	(cond
		((eq valr 'I) (setq vald 1))
		((eq valr 'V) (setq vald 5))
		((eq valr 'X) (setq vald 10))
		((eq valr 'L)(setq vald 50))
		((eq valr 'C) (setq vald 100))
		((eq valr 'D) (setq vald 500))
		((eq valr 'M) (setq vald 1000))
		(t (setq vald Nil)))
	;;(print vald) ; testing
	;;(print last-val) ; testing
	;; Recurse based on current Atom and Last atom
	(if last-val
		;; last-val != Nil
		(cond
			((eq vald Nil) 0)
			((>= vald last-val) (+ vald (r2d-recurse (cdr l) vald)))
			(t (+ (* vald -1) (r2d-recurse (cdr l)))))
		;; last-val == Nil
		(cond
			((eq vald Nil) 0)
			(t (+ vald (r2d-recurse (cdr l) vald))))))