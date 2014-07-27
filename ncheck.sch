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
      (null? 1 1 (atom))
      (= 2 1 (ceq))
      (> 2 1 (cgt))
      (>= 2 1 (cgte))
      (debug 1 0 (dbug))
      (break 0 0 (brk))
      (=0 1 1 (ldc 0) (ceq)))))

(define primop-name car)
(define primop-arity-in cadr)
(define primop-arity-out caddr)
(define primop-insns cdddr)

(define (check-namelist nl)
  (unless
      (and (list? nl)
	   (let loop ((nl nl))
	     (or (null? nl)
		 (and (symbol? (car nl))
		      (not (memq (car nl) (cdr nl)))
		      (loop (cdr nl))))))
    (error "not a list of unique names:" nl)))

(define (check-expr exp) ; -> arity
  (cond
   ((integer? exp)
    (unless (and (>= exp #x-80000000) (<= exp #x7fffffff))
      (error "integer out of range:" exp))
    1)

   ((symbol? exp)
    1)

   ((or (null? exp) (not (list? exp)))
    (error "unrecognized expression:" exp))

   ((and (= (length exp) 2) (assq (car exp) expr-primops)) =>
    (lambda (opinfo)
      (let* ((arg (cadr exp))
	     (arity-in (check-expr arg))
	     (needed (primop-arity-in opinfo)))
	(unless (if (>= needed 0) (= arity-in needed) (>= arity-in (- needed)))
	  (error "operator arity mismatch:" exp))
	(primop-arity-out opinfo))))

   ((and (eq? (car exp) '&) (list? (cdr exp)))
    (for/sum ((exp (cdr exp))) (check-expr exp)))

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

   ((and (memq (car exp) '(lambda lambda/ffi)) (list? exp) (= (length exp) 3))
    (check-namelist (cadr exp))
    (check-stmt (caddr exp) (eq? (car exp) 'lambda/ffi))
    1)

   ((and (eq? (car exp) 'set) (list? exp) (= (length exp) 3))
    (let* ((vars (cadr exp))
	   (init (caddr exp)))
      (check-namelist vars)
      (unless (= (check-expr init) (length vars))
	(error "mutation arity mismatch:" exp))
      0))

   (else
    (error "unrecognized expression:" exp))))

(define (expr-cost exp (join? #f))
  (cond
   ((or (integer? exp)
	(symbol? exp)
	(memq (car exp) '(lambda lambda/ffi))) 1)
   ((eq? (car exp) '&)
    (for/sum ((exp (cdr exp))) (expr-cost exp)))
   ((assq (car exp) expr-primops) =>
    (lambda (opinfo)
      (+ (expr-cost (cadr exp))
	 (length (primop-insns opinfo)))))
   ((eq? (car exp) 'set)
    (+ (length (cadr exp))
       (expr-cost (caddr exp))))
   ((eq? (car exp) 'if)
    (+ (if join? 1 2)
       (expr-cost (cadr exp))
       (max (expr-cost (caddr exp) #t)
	    (expr-cost (cadddr exp) #t))))
   (else
    (error "unrecognized expression:" exp))))



(define (check-stmt stmt (ffi? #f))
  (cond
   ((or (null? stmt) (not (list? stmt)))
    (error "unrecognized statement:" stmt))

   ((and (eq? (car stmt) 'ret) (= (length stmt) 2))
    (when ffi?
      (error "not allowed in FFI context:" stmt))
    (check-expr (cadr stmt))
    (void))

   ((and (eq? (car stmt) 'ret/ffi) (= (length stmt) 2))
    (unless ffi?
      (error "allowed only in FFI context:" stmt))
    (unless (= (check-expr (cadr stmt)) 1)
      (error "FFI return value not unary:" (cadr stmt)))
    (void))

   ((and (eq? (car stmt) 'goto) (= (length stmt) 3))
    (when ffi?
      (error "not allowed in FFI context:" stmt))
    (unless (= (check-expr (cadr stmt)) 1)
      (error "function expression not unary:" (cadr stmt)))
    (check-expr (caddr stmt))
    (void))

   ((and (eq? (car stmt) 'bind) (= (length stmt) 3))
    (check-bind (cadr stmt))
    (check-stmt (caddr stmt) ffi?))

   ((and (eq? (car stmt) 'seq) (= (length stmt) 3))
    (unless (zero? (check-expr (cadr stmt)))
      (error "non-nullary expression used for effect only:" (cadr stmt)))
    (check-stmt (caddr stmt) ffi?))

   ((and (eq? (car stmt) 'if) (= (length stmt) 4))
    (unless (= (check-expr (cadr stmt)) 1)
      (error "condition guard not unary:" stmt))
    (check-stmt (caddr stmt) ffi?)
    (check-stmt (cadddr stmt) ffi?))

   (else
    (error "unrecognized statement:" stmt))))


(define (check-bind bind)
  (cond
   ((not (list? bind))
    (error "unrecognized binding:" bind))

   ((and (memq (car bind) '(var rec)) (odd? (length bind)))
    (let loop ((stuff (cdr bind)))
      (unless (null? stuff)
	(check-namelist (car stuff))
	(unless (= (length (car stuff)) (check-expr (cadr stuff)))
	  (error "arity mismatch in binding:"
		 (list (car bind) (car stuff) (cadr stuff))))
	(loop (cddr stuff)))))

   ((and (eq? (car bind) 'call) (= (length bind) 4))
    (check-namelist (cadr bind))
    (unless (= (check-expr (caddr bind)) 1)
      (error "function expression not unary:" (cadr stmt)))
    (check-expr (cadddr bind))
    (void))

   (else
    (error "unrecognized binding:" bind))))


(define (check-toplevel tl)
  (check-stmt tl #t))

