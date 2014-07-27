(load "block.sch") ;; FIXME

(define cmplr-ndebug (make-parameter #f))

(define (cmplr block env exp tail)
  (define (emit . args) (apply block-emit block args))
  (define (recur exp) (cmplr block env exp '()))
  (define (tail-used!) (set! tail '()))
  (define (recur/tail exp)
    (cmplr block env exp tail)
    (tail-used!))
  (define (tail-drop?) (and (pair? tail) (eq? (car tail) 'drop)))
  (define (tail-proper?)
    (let loop ((tail tail))
      (if (pair? tail) (loop (cdr tail))
	  (not (null? tail)))))
  (define (for-effect!)
    (if (tail-drop?)
	(set! tail (cdr tail))
	(emit 'ldc #xDEADBEEF)))

  (define (do-envop ldst sym)
    (let floop ((env env) (n 0))
      (when (null? env)
	(error "unbound variable:" sym))
      (let vloop ((frame (car env)) (i 0))
	(cond
	 ((null? frame) (floop (cdr env) (+ n 1)))
	 ((eq? (car frame) sym) (emit ldst n i))
	 (else (vloop (cdr frame) (+ i 1)))))))

  (define (do-binop op exps)
    (recur (car exps))
    (for ((exp (in-list (cdr exps))))
      (recur exp)
      (emit op)))
  (define (do-addmul op unit exps)
    (if (null? exps) (emit 'ldc unit)
	(do-binop op exps)))
  (define (do-sub exps)
    (cond
     ((null? exps) (error "- needs arguments"))
     ((null? (cdr exps)) (do-sub (cons 0 exps)))
     (else (do-binop 'sub exps))))
  (define (do-div exps)
    (cond
     ((null? exps) (error "/ needs arguments"))
     ((null? (cdr exps)) (error "/ needs more arguments"))
     (else (do-binop 'div exps))))

  (cond
   ((integer? exp) (emit 'ldc exp))
   ((symbol? exp) (do-envop 'ld exp))

   ((cons? exp)
    (case (car exp)
      ((+) (do-addmul 'add 0 (cdr exp)))
      ((-) (do-sub (cdr exp)))
      ((*) (do-addmul 'mul 1 (cdr exp)))
      ((/) (do-div (cdr exp)))
      ((= < > <= >= cons)
       (unless (= (length exp) 3)
	 (error "wrong arity for binary operator:" exp))
       (let ((lhs (cadr exp))
	     (rhs (caddr exp)))
	 (case (car exp)
	   ((=) (recur lhs) (recur rhs) (emit 'ceq))
	   ((>) (recur lhs) (recur rhs) (emit 'cgt))
	   ((<) (recur rhs) (recur lhs) (emit 'cgt))
	   ((>=) (recur lhs) (recur rhs) (emit 'cgte))
	   ((<=) (recur rhs) (recur lhs) (emit 'cgte))
	   ((cons) (recur lhs) (recur rhs) (emit 'cons)))))
      ((atom? car cdr)
       (unless (= (length exp) 2)
	 (error "wrong arity for unary operator:" exp))
       (recur (cadr exp))
       (let ((op (car exp)))
	 (emit (if (eq? op 'atom?) 'atom op))))

      ((if)
       (unless (= (length exp) 4)
	 (error "wrong arity for conditional:" exp))
       (if (and (pair? (cadr exp)) (eq? (caadr exp) 'not))
	   (recur/tail `(if ,(cadadr exp) ,(cadddr exp) ,(caddr exp)))
	   (let ((>then (block-fork block))
		 (>else (block-fork block)))
	     (recur (cadr exp))
	     (let-values
		 (((op new-tail)
		   (if (tail-proper?)
		       (values 'tsel tail)
		       (values 'sel (append tail 'join)))))
	       (emit op >then >else)
	       (cmplr >then env (caddr exp) new-tail)
	       (cmplr >else env (cadddr exp) new-tail)
	       (tail-used!)))))
      ((lambda)
       (unless (and (list? (cadr exp)) (andmap symbol? (cadr exp)))
	 (error "unsupported argument list:" exp))
       (let ((>body (block-fork block)))
	 (emit 'ldf >body)
	 (cmplr >body (cons (cadr exp) env) `(begin ,@(cddr exp)) 'rtn)))
      ((funcall)
       (for-each recur (cddr exp))
       (recur (cadr exp))
       (if (eq? tail 'rtn)
	   (begin
	     (emit 'tap (length (cddr exp)))
	     (tail-used!))
	   (emit 'ap (length (cddr exp)))))
      ((letrec)
       (for ((vi (in-list (cadr exp))))
	 (unless (= (length vi) 2)
	   (error "not an identifier and expression:" vi)))
       (let ((vars (map car (cadr exp)))
	     (inits (map cadr (cadr exp)))
	     (body (cddr exp)))
	 (emit 'dum (length vars))
	 (for-each (lambda (init) (cmplr block (cons vars env) init '())) inits)
	 (recur `(lambda ,vars ,@body))
	 (if (eq? tail 'rtn)
	     (begin
	       (emit 'trap (length vars))
	       (tail-used!))
	     (emit 'rap (length vars)))))

      ((begin)
       (let ((stmts (cdr exp)))
	 (cond
	  ((null? stmts)
	   (error "begin: empty form not allowed"))
	  ((and (pair? (car stmts)) ; Splicing begin.
		(eq? (caar stmts) 'begin))
	   (recur/tail `(begin ,@(cdar stmts) ,@(cdr stmts))))
	  ((null? (cdr stmts)) ; Unary begin.
	   (recur/tail (car stmts)))
	  (else
	   (cmplr block env (car stmts) '(drop))
	   (recur/tail `(begin ,@(cdr stmts)))))))

      ((void)
       (unless (null? (cdr exp))
	 (error "the void stares also into you" exp))
       (for-effect!))

      ((set!)
       (unless (= (length exp) 3)
	 (error "wrong arity for set!:" exp))
       (recur (caddr exp))
       (do-envop 'st (cadr exp))
       (for-effect!))

      ((debug!)
       (for ((arg (in-list (cdr exp))))
	 (recur arg)
	 (emit 'dbug))
       (for-effect!))
      ((break!)
       (emit 'brk)
       (for-effect!))
      ((assert!)
       (if (cmplr-ndebug)
	   (recur/tail '(void))
	   (recur/tail `(unless ,(cadr exp)
			  (debug! ,@(cddr exp))
			  (break!)))))

      ;; fake macros
      ((let)
       (for ((vi (in-list (cadr exp))))
	 (unless (= (length vi) 2)
	   (error "not an identifier and expression:" vi)))
       (let ((vars (map car (cadr exp)))
	     (inits (map cadr (cadr exp)))
	     (body (cddr exp)))
	 (recur/tail `(funcall (lambda ,vars ,@body) ,@inits))))
      ((let*)
       (if (null? (cadr exp)) (recur (caddr exp))
	   (recur/tail `(let (,(caadr exp)) (let* ,(cdadr exp) ,@(cddr exp))))))
      ((cond)
       (cond
	((null? (cdr exp))
	 (for-effect!))
	((eq? (caadr exp) 'else)
	 ;; It's a little unhygienic to treat else like that.
	 ;; But I *know* if I require #t I'll get it wrong & be annoyed.
	 (recur/tail `(begin ,@(cdadr exp))))
	(else (recur/tail `(if ,(caadr exp)
			       (begin ,@(cdadr exp))
			       (cond ,@(cddr exp)))))))
      ((when)
       (recur/tail `(if ,(cadr exp) (begin ,@(cddr exp)) (void))))
      ((unless)
       (recur/tail `(when (not ,(cadr exp)) ,@(cddr exp))))

      ((not)
       (recur/tail `(= 0 ,@(cdr exp))))
      ; Would be nice to be able to define pair? to be expanded *before* the if-not hack.

      (else (error "unhandled operator:" exp))))
   (else (error "unhandled expression:" exp)))
  (when (tail-drop?)
    (let ((>trash (block-fork block)))
      (emit 'ldf >trash)
      (emit 'ap 1)
      (block-emit >trash 'rtn))
    (set! tail (cdr tail)))
   (unless (null? tail)
     (emit tail)))

(define (cmplr/prog exp (with-labels #f))
  (let ((main (new-block)))
    (cmplr main '((initial-world undocumented)) exp 'rtn)
    (for-each display (block->strings main with-labels))))

;; FIXME: this should unwind-protect (or whatever) the open file.
(define (cmplr/xclip exp)
  (let ((xclip (cadr (process "xclip -i"))))
    (parameterize ((current-output-port xclip)) (cmplr/prog exp))
    (close-output-port xclip)))

(define (cmplr/file fn exp)
  (with-output-to-file fn
    (lambda () (cmplr/prog exp))
    #:exists 'replace))
