(load "ncheck.sch") ;; FIXME

(define (classify-core-form form)
  (if (not (pair? form)) 'expr
      (case (car form)
	((& set lambda class
	    + - * / cons car cdr atom = > >= debug break =0)
	 'expr)
	((ret halt goto bind seq)
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
      ((ret halt + - * / cons car cdr null? = > >= debug break =0)
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
      
      ;; Macro-like things:
      ((defun)
       (let ((name (cadr form)) (args (caddr form)) (body (cdddr form)))
	 (let ((cname (gensym name)))
	   `(rec ((: ,name ,cname)) (class ,cname ,(fix-decl-list args) ,(expand-begin body))))))

      ;; Fake primitives:
      ((<) (expand `(=0 (>= ,@(cdr form)))))
      ((<=) (expand `(=0 (> ,@(cdr form)))))
      ((<>) (expand `(=0 (= ,@(cdr form)))))
      ((thing?) (expand `(=0 (null? ,@(cdr form)))))

      (else
       (error "unexpanded form" form))
     ))))
