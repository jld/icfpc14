(load "block.sch") ;; FIXME

(define (env-lookup env var)
  (let floop ((env env) (n 0))
    (when (null? env)
      (error "unbound variable:" var))
    (let vloop ((frame (car env)) (i 0))
      (cond
       ((null? frame) (floop (cdr env) (+ n 1)))
       ((eq? (car frame) var) (values n i))
       (else (vloop (cdr frame) (+ i 1)))))))

(define expr-primops
  (let ()
    `((+ 2 1 (add))
      (- 2 1 (sub))
      (* 2 1 (mul))
      (/ 2 1 (div))
      (cons 2 1 (cons))
      (car 1 1 (car))
      (cdr 1 1 (cdr))
      (atom? 1 1 (atom))
      (= 2 1 (ceq))
      (> 2 1 (cgt))
      (>= 2 1 (cgte))
      (debug! 1 0 (dbug))
      (break! 0 0 (brk))
      (not 1 1 (ldc 0) (ceq)))))

(define primop-name car)
(define primop-arity-in cadr)
(define primop-arity-out caddr)
(define primop-insns cdddr)

(define (check-expr exp) ; -> arity
  (cond
   ((integer? exp)
    (unless (and (>= exp #x-80000000) (<= exp #x7fffffff))
      (error "integer out of range:" exp))
    1)

   ((symbol? exp)
    1)

   ((not (list? exp))
    (error "unrecognized expression:" exp))

   ((and (= (length exp) 2) (assq (car exp) expr-primops)) =>
    (lambda (opinfo)
      (let* ((arg (cadr exp))
	     (arity-in (check-expr arg))
	     (needed (primop-arity-in opinfo)))
	(unless (if (>= needed 0) (= arity-in needed) (>= arity-in (- needed)))
	  (error "operator arity mismatch:" exp))
	(primop-arity-out opinfo))))

   ((and (eq? (car exp) '$$) (list? (cdr exp)))
    (for/sum ((exp (cdr exp))) (expr-arity exp)))

   ((and (eq? (car exp) 'if) (list? exp) (= (length exp) 4))
     (let ((predic (cadr exp))
	   (conseq (caddr exp))
	   (altern (cadddr exp)))
       (unless (= (check-expr predic) 1)
	 (error "condition guard not unary:" exp))
       (let ((arity-conseq (check-expr conseq))
	     (arity-altern (check-expr altern)))
	 (unless (= arity-conseq arity-altern)
	   (error "conditional arity mismatch:" exp))
	 arity-conseq)))

   ((and (eq? (car exp) 'lambda) (list? exp) (= (length exp) 3) (list? (cadr exp)))
    1)
   
   ((and (eq? (car exp) 'set!) (list? exp) (= (length exp) 3) (list? (cadr exp)))
    (let* ((vars (cadr exp))
	   (init (caddr exp)))
      (unless (= (check-expr init) (length vars))
	(error "mutation arity mismatch:" exp))
      0))

   (else
    (error "unrecognized expression:" exp))))

(define cc-name '*continuation*)

(define (compile-expr block env exp (join? #f))
  (define (emit . args) (apply block-emit block args))
  (define (recur exp) (compile-expr block env exp))

  (cond
   ((integer? exp)
    (emit 'ldc exp))

   ((symbol? exp)
    (let-values (((frame slot) (env-lookup env exp)))
      (emit 'ld frame slot)))

   ((assq (car exp) expr-primops) =>
    (lambda (opinfo)
      (recur (cadr exp))
      (for ((insn (primop-insns opinfo)))
	(apply emit insn))))

   ((eq? (car exp) '$$)
    (for-each recur (cdr exp)))

   ((eq? (car exp) 'if)
    (let ((>then (block-fork block))
	  (>else (block-fork block)))
      (recur (cadr exp))
      (emit (if join? 'tsel 'sel) >then >else)
      (compile-expr >then env (caddr exp) #t)
      (compile-expr >else env (cadddr exp) #t)
      (set! join? #f)))

   ;; lambda
   ((eq? (car exp) 'lambda)
    (let ((body (compile-stmt (cons (cons cc-name (cadr exp)) env) (cddr exp))))
      (emit 'ldf body)))

   ;; set!
   ((eq? (car exp) 'set!)
    (let ((vars (cadr exp))
	  (init (caddr exp)))
      (recur init)
      (for ((var (reverse vars)))
	(let-values (((frame slot) (env-lookup env var)))
	  (emit 'st frame slot)))))

   (else
    (error "internal error: unrecognized expression:" exp)))

  (when join? (emit 'join)))

(define (dump-expr exp (env '()))
  (let ((main (new-block)))
    (printf "; arity = ~a~n" (check-expr exp))
    (compile-expr main env exp)
    (for-each display (block->strings main #t))))
  
