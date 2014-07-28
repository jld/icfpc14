(load "block.sch") ;; FIXME
(load "ncheck.sch") ;; FIXME

(define cc-name '*continuation*)

;; The only thing this needs the tcx for, rather than just the env,
;; are a couple of places it needs to know arity.

(define (env-access tcx emit ldst var)
  (let-values (((n i t) (env-lookup (tcx-env tcx) var))) (emit ldst n i)))

(define (compile-expr block tcx exp (join? #f))
  (define (emit . args) (apply block-emit block args))
  (define (recur exp) (compile-expr block tcx exp))
  (cond

   ((integer? exp)
    (emit 'ldc exp))

   ((symbol? exp)
    (env-access tcx emit 'ld exp))

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
	 (compile-expr >then tcx (caddr exp) #t)
	 (compile-expr >else tcx (cadddr exp) #t)
	 (set! join? #f)))

      ((class lambda)
       (let* ((class? (eq? (car exp) 'class))
	      (frame (if class? (cons cc-name (caddr exp)) (cadr exp)))
	      (body (if class? (cadddr exp) (caddr exp)))
	      (>body (block-fork block)))
	 (compile-stmt >body (tcx-nest tcx frame) body)
	 (emit 'ldf >body)))

      ((set)
       (let ((vars (cadr exp))
	     (init (caddr exp)))
	 (recur init)
	 (for ((var (reverse vars)))
	   (env-access tcx emit 'st var))))

      (else
       (error "internal error: unrecognized expression:" exp)))))

  (when join? (emit 'join)))

(define (dump-expr exp (env '()))
  (let ((main (new-block))
	(tcx (new-tcx env)))
    (printf "; types = ~a~n" (check-expr tcx exp))
    (compile-expr main tcx exp)
    (for-each display (block->strings main #t))))


(define (compile-stmt block tcx stmt)
  (define (emit . args) (apply block-emit block args))
  (case (car stmt)

    ((ret)
     (compile-expr block tcx (cadr stmt))
     (env-access tcx emit 'ld cc-name)
     (emit 'tap (length (check-expr tcx (cadr stmt))))) ; FIXME: double-traversal

    ((halt)
     (compile-expr block tcx (cadr stmt))
     (emit 'rtn))

    ((goto)
     (compile-expr block tcx (caddr stmt))
     (env-access tcx emit 'ld cc-name)
     (compile-expr block tcx (cadr stmt))
     (emit 'tap (+ 1 (length (check-expr tcx (caddr stmt)))))) ; FIXME: double-traversal

    ((bind)
     (let ((>next (block-fork block))
	   (args (bind-args (cadr stmt))))
       (compile-bind block >next tcx (cadr stmt))
       (compile-stmt >next (tcx-nest tcx args) (caddr stmt))))

    ((seq)
     (compile-expr block tcx (cadr stmt))
     (compile-stmt block tcx (caddr stmt)))

    ((if)
     (let ((>then (block-fork block))
	   (>else (block-fork block)))
       (compile-expr block tcx (cadr stmt))
       (emit 'tsel >then >else)
       (compile-stmt >then tcx (caddr stmt))
       (compile-stmt >else tcx (cadddr stmt))))

    (else
     (error "internal error: unrecognized statement:" stmt))))

(define (dump-stmt stmt (env '()))
  (let ((tcx (new-tcx env))
	(main (new-block))
	(cln (gensym)))
    (check-stmt tcx cln stmt)
    (compile-stmt main tcx stmt)
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

(define (compile-bind block >next tcx bind)
  (define (emit . args) (apply block-emit block args))
  (case (car bind)

    ((var rec)
     (let* ((rec? (eq? (car bind) 'rec))
	    (args (bind-args bind))
	    (init-tcx (if rec? (tcx-nest tcx args) tcx))
	    (num-args (length args)))
       (when rec? (emit 'dum num-args))
       (let loop ((stuff (cdr bind)))
	 (unless (null? stuff)
	   (compile-expr block init-tcx (cadr stuff))
	   (loop (cddr stuff))))
       (emit 'ldf >next)
       (emit (if rec? 'trap 'tap) num-args)))

    ((call)
     (compile-expr block tcx (cadddr bind))
     (emit 'ldf >next)
     (compile-expr block tcx (caddr bind))
     (emit 'tap (+ 1 (check-expr (cadddr bind))))) ; FIXME: double-traversal

    (else
     (error "internal error: unrecognized binding:" bind))))


(define (compile-toplevel block tcx tl)
  (compile-stmt block tcx tl))

(define (dump-toplevel tl (env '()))
  (let ((main (new-block))
	(tcx (new-tcx env)))
    (check-toplevel tcx tl)
    (compile-toplevel main tcx tl)
    (for-each display (block->strings main #t))))
