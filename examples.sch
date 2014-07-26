(define randwalk
  `(cons 17 (lambda (s w)
	      (cons (+ 12345 (* s 1103515245))
		    (if (< s 0) (- 3 (/ s ,(- (expt 2 30)))) 
			(/ s ,(expt 2 30)))))))
