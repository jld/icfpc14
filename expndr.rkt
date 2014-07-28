(load "ncheck.rkt") ;; FIXME

(define (classify-core-form form)
  (if (not (pair? form)) 'expr
      (case (car form)
	((& set lambda class
	    + - * / cons car cdr atom = > >= debug break =0)
	 'expr)
	((ret halt goto bind seq declaring)
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
	   (if (null? (cdr things)) t0
	       `(bind (call () (class ,(gensym 'block) () ,t0) (&))
		      ,(expand-begin (cdr things)))))
	  (else
	   (case (car t0)
	     ((flatten) (expand-begin (append (cdr t0) (cdr things))))
	     ((declare)
	      (if (null? (cdr t0)) (expand-begin (cdr things))
		  (expand `(declaring ,(cadr t0) (begin (declare ,@(cddr t0))
							,@(cdr things))))))
	     (else (error "unknown expanded begin item:" t0))))))))


(define (fix-expr-list el)
  (if (= (length el) 1) (car el) `(& ,@el)))

(define (fix-decl-list vl)
  (if (decl? vl) (list vl) vl))

(define (fix-var-list vl)
  (if (symbol? vl) (list vl) vl))

(define (expand form)
  (cond
   ((not (pair? form)) form)
   (else
    (case (car form)
      ;; Core (and core-ish) forms:
      ((begin)
       (expand-begin (cdr form)))
      ((ret halt + - * / cons car cdr null? = > >= debug break =0 unsafe)
       `(,(car form) ,(fix-expr-list (map expand (cdr form)))))
      ((&)
       `(& ,@(map expand (cdr form))))
      ((set)
       `(set ,(fix-decl-list (cadr form))
	     ,(fix-expr-list (map expand (cddr form)))))
      ((lambda)
       `(,(car form) ,(fix-decl-list (cadr form)) ,(expand-begin (cddr form))))
      ((class)
       `(,(car form) ,(cadr form) ,(fix-decl-list (caddr form)) ,(expand-begin (cdddr form))))
      ((goto)
       `(goto ,(expand (cadr form)) ,(fix-expr-list (map expand (cddr form)))))
      ((seq bind)
       `(,(car form) ,(expand (cadr form)) ,(expand-begin (cddr form))))
      ((if)
       (let loop ((predic (expand (cadr form)))
		  (conseq (expand (caddr form)))
		  (altern (expand (cadddr form))))
	 (if (and (pair? predic) (eq? (car predic) '=0))
	     (loop (cadr predic) altern conseq)
	     `(if ,predic ,conseq ,altern))))
      ((var rec)
       `(,(car form)
	 ,@(let loop ((stuff (cdr form)))
	     (if (null? stuff) '()
		 `(,(fix-decl-list (car stuff))
		   ,(expand (cadr stuff))
		   ,@(loop (cddr stuff)))))))
      ((call)
       `(call ,(fix-decl-list (cadr form))
	      ,(expand (caddr form))
	      ,(fix-expr-list (map expand (cdddr form)))))
      ((declaring)
       `(declaring ,(cadr form) ,(expand-begin (cddr form))))
      ((declare)
       form)

      ;; Macro-like things:
      ((defun)
       (let ((name (cadr form)) (args (caddr form)) (body (cdddr form)))
	 (let ((cname (gensym name)))
	   `(rec ((: ,name ,cname)) (class ,cname ,(fix-decl-list args) ,(expand-begin body))))))

      ((untuple)
       ; Be careful with this; it duplicates the expression.
       (let ((n (cadr form)))
	 (unless (and (integer? n) (> n 0))
	   (error "untuple needs a static positive integer"))
	 `(& ,@(let loop ((n n) (exp (fix-expr-list (map expand (cddr form)))))
		 (if (= n 1) `(,exp)
		     `((car ,exp)
		       ,@(loop (- n 1) `(cdr ,exp))))))))
      ((tuple)
       (expand
	(let loop ((args (cdr form)))
	  (cond
	   ((null? args) (error "0-tuples don't exist"))
	   ((null? (cdr args)) (car args))
	   (else `(cons ,(car args) (tuple ,@(cdr args))))))))

      ((!nth)
       (let ((n (cadr form)))
	 (unless (integer? n)
	   (error "!nth needs a static integer"))
	 (if (zero? n) (expand `(car ,@(cddr form)))
	     (expand `(!nth ,(- n 1) (cdr ,@(cddr form)))))))

      ;; Fake primitives:
      ((<) (expand `(=0 (>= ,@(cdr form)))))
      ((<=) (expand `(=0 (> ,@(cdr form)))))
      ((<>) (expand `(=0 (= ,@(cdr form)))))
      ((thing?) (expand `(=0 (null? ,@(cdr form)))))

      ;; Control-ish things
      ((block)
       (let ((vars (cadr form))
	     (body (cddr form))
	     (cname (gensym 'block)))
	 ;; This is inefficient...
	 (expand `(call ,vars (class ,cname () ,@body)))))
      ((when)
       (expand `(if ,(cadr form) (begin ,@(cddr form)) (ret))))
      ((unless)
       (expand `(when (=0 ,(cadr form)) ,@(cddr form))))

      ((cond)
       (cond
	((null? (cdr form)) '(ret))
	((eq? (caadr form) 'else)
	 (expand `(begin ,@(cdadr form))))
	(else (expand `(if ,(caadr form)
			   (begin ,@(cdadr form))
			   (cond ,@(cddr form)))))))
      ((and)
       (cond
	((null? (cdr form)) 1)
	((null? (cddr form)) (expand (cadr form)))
	(else (expand `(if ,(cadr form) (and ,@(cddr form)) 0)))))
      ((or)
       (cond
	((null? (cdr form)) 0)
	((null? (cddr form)) (expand (cadr form)))
	(else (expand `(if ,(cadr form) 1 (or ,@(cddr form)))))))

      (else
       (error "unexpanded form" form))
     ))))
