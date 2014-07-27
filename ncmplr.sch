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
      (debug 1 0 (dbug))
      (break 0 0 (brk))
      (not 1 1 (ldc 0) (ceq)))))

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

   (else
    (case (car exp)
      ((&)
       (for-each recur (cdr exp)))

      ((if)
       (let ((>then (block-fork block))
	     (>else (block-fork block)))
	 (recur (cadr exp))
	 (emit (if join? 'tsel 'sel) >then >else)
	 (compile-expr >then env (caddr exp) #t)
	 (compile-expr >else env (cadddr exp) #t)
	 (set! join? #f)))

      ((lambda lambda/ffi)
       (let ((>body (block-fork block))
	     (frame (if (eq? (car exp) 'lambda)
			(cons cc-name (cadr exp))
			(cadr exp))))
	 (compile-stmt >body (cons frame env) (caddr exp))
	 (emit 'ldf >body)))

      ((set)
       (let ((vars (cadr exp))
	     (init (caddr exp)))
	 (recur init)
	 (for ((var (reverse vars)))
	   (let-values (((frame slot) (env-lookup env var)))
	     (emit 'st frame slot)))))

      (else
       (error "internal error: unrecognized expression:" exp)))))

  (when join? (emit 'join)))

(define (dump-expr exp (env '()))
  (let ((main (new-block)))
    (printf "; arity = ~a~n" (check-expr exp))
    (compile-expr main env exp)
    (for-each display (block->strings main #t))))


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

(define (compile-stmt block env stmt)
  (define (emit . args) (apply block-emit block args))
  (case (car stmt)

    ((ret)
     (compile-expr block env (cadr stmt))
     (compile-expr block env cc-name)
     (emit 'tap (check-expr (cadr stmt)))) ; FIXME: double-traversal

    ((ret/ffi)
     (compile-expr block env (cadr stmt))
     (emit 'rtn))

    ((goto)
     (compile-expr block env (caddr stmt))
     (compile-expr block env cc-name)
     (compile-expr block env (cadr stmt))
     (emit 'tap (+ 1 (check-expr (caddr stmt))))) ; FIXME: double-traversal

    ((bind)
     (let ((>next (block-fork block))
	   (args (bind-args (cadr stmt))))
       (compile-bind block >next env (cadr stmt))
       (compile-stmt >next (cons args env) (caddr stmt))))

    ((seq)
     (compile-expr block env (cadr stmt))
     (compile-stmt block env (caddr stmt)))

    ((if)
     (let ((>then (block-fork block))
	   (>else (block-fork block)))
       (compile-expr block env (cadr stmt))
       (emit 'tsel >then >else)
       (compile-stmt >then env (caddr stmt))
       (compile-stmt >else env (cadddr stmt))))

    (else
     (error "internal error: unrecognized statement:" stmt))))

(define (dump-stmt stmt (env '()))
  (let ((main (new-block)))
    (check-stmt stmt)
    (compile-stmt main env stmt)
    (for-each display (block->strings main #t))))

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

(define (bind-args bind)
  (case (car bind)
    ((call)
     (cadr bind))
    ((var rec)
     (let loop ((stuff (cdr bind)))
       (if (null? stuff) '()
	   (append (car stuff) (loop (cddr stuff))))))
    (else
     (error "internal error: unrecognized binding:" bind))))

(define (compile-bind block >next env bind)
  (define (emit . args) (apply block-emit block args))
  (case (car bind)

    ((var rec)
     (let* ((rec? (eq? (car bind) 'rec))
	    (args (bind-args bind))
	    (init-env (if rec? (cons args env) env))
	    (num-args (length args)))
       (when rec? (emit 'dum num-args))
       (let loop ((stuff (cdr bind)))
	 (unless (null? stuff)
	   (compile-expr block init-env (cadr stuff))
	   (loop (cddr stuff))))
       (emit 'ldf >next)
       (emit (if rec? 'trap 'tap) num-args)))

    ((call)
     (compile-expr block env (cadddr bind))
     (emit 'ldf >next)
     (compile-expr block env (caddr bind))
     (emit 'tap (+ 1 (check-expr (cadddr bind))))) ; FIXME: double-traversal

    (else
     (error "internal error: unrecognized binding:" bind))))


(define (check-toplevel tl)
  (check-stmt tl #t))

(define (compile-toplevel block env tl)
  (compile-stmt block env tl))

(define (dump-toplevel tl (env '()) (req-arity 1))
  (let ((main (new-block)))
    (check-toplevel tl)
    (compile-toplevel main env tl)
    (for-each display (block->strings main #t))))
