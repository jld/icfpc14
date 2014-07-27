(define (expand thing)
  (cond
   ((and (pair? thing) (assq (car thing) (expanders))) =>
    (lambda (exp)
      (let ((new-thing ((cadr exp) (cdr thing))))
	(if (eq? (car thing) (car new-thing))
	    new-thing
	    (expand new-thing)))))
   (else thing)))

(define (core-form-expandable? form)
  (and (pair? form)
       (case (car form)
	 ((& if ret ret/ffi goto bind seq
	     + - * / cons car cdr atom = > >= debug break not)
	  '(#f . #t))
	 ((set lambda lambda/ffi)
	  '(#f #f #t))
	 ((call)
	  '(#f #f #t #t))
	 ((var rec)
	  (for/list ((i naturals)
		     (_ (in-list form)))
	    (odd? i))))))

(define (expand-recursively form)
  (let loop ((exp? (core-form-expandable? form)) (form form))
    (cond
     ((eq? exp? #t) (expand form))
     ((or (not exp?) (null? exp?)) form)
     (else (cons (loop (car exp?) (car form))
		 (loop (cdr exp?) (cdr form)))))))

(define (classify-core-form form)
  (if (not (pair? form)) 'expr
      (case (car form)
	((& set lambda lambda/ffi
	    + - * / cons car cdr atom = > >= debug break not)
	 'expr)
	((ret ret/ffi goto bind seq)
	 'stmt)
	((var rec call)
	 'bind)
	((if)
	 (classify-core-form (caddr form)))
	(else
	 #f))))

(define (expand-begin things)
  (if (null? things) (expand '(ret))
      (let ((t0 (expand (car things))))
	(case (classify-core-form t0)
	  ((expr) `(seq ,t0 ,(expand-begin (cdr things))))
	  ((bind) `(bind ,t0 ,(expand-begin (cdr things))))
	  ((stmt)
	   (unless (null? (cdr things))
	     (error "premature end of begin:" `(begin ,@things)))
	   t0)
	  (else
	   (unless (eq? (car t0) 'flatten)
	     (error "unknown expanded begin item:" t0))
	   (expand-begin (append (cdr t0) (cdr things))))))))

(define (fix-expr-list el)
  (if (= (length el) 1) (car el) `(& ,@el)))

(define (fix-var-list vl)
  (if (symbol? vl) (list vl) vl))

(define the-primops '(+ - * / cons car cdr atom? = > >= debug break not))

(define expanders
  (make-parameter
   `((begin ,expand-begin)
     ,@(map (lambda (head)
	      (list head (lambda (el) (list head (fix-expr-list (map expand el))))))
	    `(ret ret/ffi ,@the-primops))
     (set ,(lambda (things)
	     `(set ,(fix-var-list (car things)) ,(fix-expr-list
						  (map expand (cdr things))))))
     ,@(map (lambda (head)
	      (list head (lambda (things)
			   (list head (fix-var-list (car things)) (expand-begin (cdr things))))))
	    '(lambda lambda/ffi))
     ;; More stuff goes here.
     )))
	    


