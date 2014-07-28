(define 8bit '(declare (max-depth 256)))

(define (nboot inits statevar worldvar mains newstate dir)
  `(begin
     ,@inits
     (halt (cons 0 (lambda (,statevar ,worldvar)
		     ,@mains 
		     (halt (cons ,newstate ,dir)))))))

(define nsimplesearch
  (nboot
   `((defun drop ((: n int) l) ,8bit
       (if (=0 n) (ret l)
	   (goto drop (- n 1) (cdr l)))))
   '(: lastd int) 'world
   `((var (wmap lman ghos fruit) (untuple 4 world))
     (var (lmvit lmloc lmdir lmliv lmsc) (untuple 5 lman))
     (var ((: x int) (: y int)) (unsafe (untuple 2 lmloc)))
     (call chop drop (- y 1) wmap)
     (call upchop drop x (!nth 0 chop))
     (call thischop drop (- x 1) (!nth 1 chop))
     (call downchop drop x (!nth 2 chop))
     (var (: sq0 int) (unsafe (!nth 0 upchop))
	  (: sq1 int) (unsafe (!nth 2 thischop))
	  (: sq2 int) (unsafe (!nth 0 downchop))
	  (: sq3 int) (unsafe (!nth 0 thischop)))
     (var ((: b0 int) (: b1 int) (: b2 int) (: b3 int))
	  (& ,@(for/list ((sq '(sq0 sq1 sq2 sq3)) (d (in-naturals)))
		 `(+ (if (>= lastd ,d) ,(+ 4 d) ,d) (* 8 (if (> ,sq 4) 1 ,sq))))))
     (var (: b01 int) (if (> b0 b1) b0 b1)
	  (: b23 int) (if (> b2 b3) b2 b3))
     (var whereunto (if (> b01 b23)
			(if (> b0 b1) 0 1)
			(if (> b2 b3) 2 3))))
   'whereunto
   'whereunto))
