(load "block.sch") ;; FIXME
(load "ncheck.sch") ;; FIXME

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

(define (compile-toplevel block env tl)
  (compile-stmt block env tl))

(define (dump-toplevel tl (env '()) (req-arity 1))
  (let ((main (new-block)))
    (check-toplevel tl)
    (compile-toplevel main env tl)
    (for-each display (block->strings main #t))))
