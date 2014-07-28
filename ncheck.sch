(define (make-decl name type) (if type (list ': name type) name))
(define (decl-name decl) (if (symbol? decl) decl (cadr decl)))
(define (decl-type decl) (if (symbol? decl) #f (caddr decl)))

(define (type? type)
  (or (not type) (symbol? type)))
(define (decl? decl)
  (or (symbol? decl)
      (and (pair? decl) (pair? (cdr decl)) (pair? (cddr decl)) (null? (cdddr decl))
	   (eq? (car decl) ':) (symbol? (cadr decl)) (type? (caddr decl)))))


(define (type-class? type)
  (and (symbol? type)
       (not (memq type '(int lambda _!_)))))

(define (subtype? l r)
  (or
   (not r)
   (eq? l '_!_)
   (eq? l r)
   (and (eq? l 'int) (type-class? r))))

(define (subtypes? l r)
  (or
   (null? r)
   (eq? l '_!_)
   (and (pair? l) (pair? r)
	(subtype? (car l) (car r))
	(subtypes? (cdr l) (cdr r)))))

(define (type-lub l r)
  (cond
   ((eq? l r) l)
   ((eq? l '_!_) r)
   ((eq? r '_!_) l)
   ((and (eq? l 'int) (type-class? r)) r)
   ((and (eq? r 'int) (type-class? l)) l)
   (else #f)))

(define (type-glb l r)
  (cond
   ((eq? l r) l)
   ((not l) r)
   ((not r) l)
   ((and (eq? l 'int) (type-class? r) l))
   ((and (eq? r 'int) (type-class? l) r))
   ((and (type-class? l) (type-class? r)) 'int)
   (else '_!_)))

(define (types-lub l r)
  (cond
   ((or (null? l) (null? r)) '())
   ((eq? l '_!_) r)
   ((eq? r '_!_) l)
   (else (cons (type-lub (car l) (car r)) (types-lub (cdr l) (cdr r))))))

(define (types-glb l r)
  (cond
   ((or (eq? l '_!_) (eq? r '_!_)) '_!_)
   ((null? l) r)
   ((null? r) l)
   (else (cons (type-glb (car l) (car r)) (types-glb (cdr l) (cdr r))))))


(define (env-lookup env var)
  (let floop ((env env) (n 0))
    (when (null? env)
      (error "unbound variable:" var))
    (let vloop ((frame (car env)) (i 0))
      (cond
       ((null? frame) (floop (cdr env) (+ n 1)))
       ((eq? (decl-name (car frame)) var) (values n i (decl-type (car frame))))
       (else (vloop (cdr frame) (+ i 1)))))))

(define (env-lookup-type env var)
  (let-values (((n i t) (env-lookup env var))) t))


(define expr-primops
  (let ()
    `((+     (int int) (int) (add))
      (-     (int int) (int) (sub))
      (*     (int int) (int) (mul))
      (/     (int int) (int) (div))
      (cons  (#f #f)   (#f)  (cons))
      (car   (#f)      (#f)  (car))
      (cdr   (#f)      (#f)  (cdr))
      (null? (#f)      (int) (atom))
      (=     (int int) (int) (ceq))
      (>     (int int) (int) (cgt))
      (>=    (int int) (int) (cgte))
      (debug (#f)      ()    (dbug))
      (break ()        ()    (brk))
      (=0    (int)     (int) (ldc 0) (ceq)))))

(define primop-name car)
(define primop-in cadr)
(define primop-out caddr)
(define primop-insns cdddr)


(define (check-namelist nl)
  (unless (list? nl)
    (error "name list isn't a list:" nl))
  (let loop ((nl nl) (ns '()) (ts '()))
    (if (null? nl) (reverse ts)
	(begin
	  (unless (decl? (car nl))
	    (error "not a variable declaration:" (car nl)))
	  (let ((n (decl-name (car nl)))
		(t (decl-type (car nl))))
	  (when (memq n ns)
	    (error (format "duplicate variable name \"~a\" in" n)
		   (append (reverse (map make-decl ns ts)) nl)))
	  (loop (cdr nl) (cons n ns) (cons t ts)))))))


(define (cn+ . xs)
  (let loop ((xs xs) (ipart 0) (hpart (hasheq)))
    (cond 
     ((null? xs) (cons ipart hpart))
     ((integer? (car xs)) (loop (cdr xs) (+ ipart (car xs)) hpart))
     ((symbol? (car xs))
      (loop (cdr xs) ipart (hash-update hpart (car xs) (lambda (x) (+ x 1)) 0)))
     (else
      (loop (cdr xs) (+ ipart (caar xs))
	    (for/fold ((nh hpart)) (((n v) (in-hash (cdar xs))))
	      (hash-update nh n (lambda (x) (+ x v)) 0)))))))


(struct clsinfo (cln al au rl ru costs depths)
	#:mutable #:transparent #:constructor-name make-clsinfo)
(define (new-clsinfo cln) (make-clsinfo cln '_!_ '() '_!_ '() '() '()))
(define (clsinfo-check! b)
  (unless (subtypes? (clsinfo-al b) (clsinfo-au b))
    (error 'clsinfo-check! "class ~a called with ~a but expects ~a"
	   (clsinfo-cln b) (clsinfo-al b) (clsinfo-au b)))
  (unless (subtypes? (clsinfo-rl b) (clsinfo-ru b))
    (error 'clsinfo-check! "class ~a returns ~a but asked for ~a"
	   (clsinfo-cln b) (clsinfo-rl b) (clsinfo-ru b))))

(struct tcx (env clsinfo lctr) #:constructor-name make-tcx)
(define (new-tcx env) (make-tcx env (make-hasheq) (box 0)))
(define (tcx-nest tcx frame) (make-tcx (cons frame (tcx-env tcx))
				       (tcx-clsinfo tcx)
				       (tcx-lctr tcx)))
(define (tcx-ref tcx id) (env-lookup-type (tcx-env tcx) id))
(define (tcx-lambda-number tcx)
  (let* ((b (tcx-lctr tcx)) (n (unbox b))) (set-box! b (+ n 1)) n))
(define (tcx-get-clsinfo tcx cln)
  (hash-ref! (tcx-clsinfo tcx) cln (lambda () (new-clsinfo cln))))

(define (clsinfo-note-class! b atys)
  (set-clsinfo-depths! b (cons +inf.0 (clsinfo-depths b)))
  (set-clsinfo-au! b (types-glb (clsinfo-au b) atys))
  (clsinfo-check! b))
(define (clsinfo-note-ret! b rtys)
  (set-clsinfo-rl! b (types-lub (clsinfo-rl b) rtys))
  (clsinfo-check! b))
(define (clsinfo-note-call! b atys rtys)
  (set-clsinfo-al! b (types-lub (clsinfo-al b) atys))
  (set-clsinfo-ru! b (types-glb (clsinfo-ru b) rtys))
  (clsinfo-check! b))

(define (tcx-note-class! tcx cln atys)
  (clsinfo-note-class! (tcx-get-clsinfo tcx cln) atys))
(define (tcx-note-ret! tcx cln rtys)
  (clsinfo-note-ret! (tcx-get-clsinfo tcx cln) rtys))
(define (tcx-note-call! tcx cln atys rtys)
  (clsinfo-note-call! (tcx-get-clsinfo tcx cln) atys rtys))
(define (tcx-note-goto! tcx fcln tcln atys)
  (let ((bf (tcx-get-clsinfo tcx fcln))
	(bt (tcx-get-clsinfo tcx tcln)))
    (clsinfo-note-call! bt atys (clsinfo-ru bf))
    (clsinfo-note-ret! bf (clsinfo-rl bt))))

(define (clsinfo-note-cost! b cost)
  (set-clsinfo-costs! b (cons cost (clsinfo-costs b))))
(define (tcx-note-cost! tcx cln cost)
  (clsinfo-note-cost! (tcx-get-clsinfo tcx cln) cost))

(define (clsinfo-note-depth! b depth)
  (set-clsinfo-depths! b (cons depth (cdr (clsinfo-depths b)))))
(define (tcx-note-depth! tcx cln depth)
  (clsinfo-note-depth! (tcx-get-clsinfo tcx cln) depth))

(define (tcx-costs tcx)
  (for/list (((cln b) (in-hash (tcx-clsinfo tcx))))
    (list cln (clsinfo-costs b) (clsinfo-depths b))))

(define (tcx-get-max-depth tcx cln)
  (foldl max 0 (clsinfo-depths (tcx-get-clsinfo tcx cln))))

(define (tcx-eval-cost tcx cln (depth-map (hasheq)))
  ;; This is horrendously inefficient and needs to be fixed.
  ;; I have a better idea, but no time right now....
  (let ((depth (hash-ref depth-map cln #f)))
    (cond 
     ((and depth (<= depth 0)) ; Deeper than declaration.
      -inf.0)
     ((and depth (infinite? depth)) ; Recursion without declaration.
      +inf.0)
     (else
      (let* ((depth (- (or depth (tcx-get-max-depth tcx cln)) 1))
	     (depth-map (hash-set depth-map cln depth)))
	(for/fold ((worst 0)) ((cost (in-list (clsinfo-costs (tcx-get-clsinfo tcx cln)))))
	  (let ((this (for/fold ((acc (car cost))) (((callee times) (in-hash (cdr cost))))
			(+ acc (* times (tcx-eval-cost tcx callee depth-map))))))
	    (if (> this worst) this worst))))))))

(define (tcx-eval-all-costs tcx)
  (for/list (((cln b) (in-hash (tcx-clsinfo tcx))))
    (list cln (tcx-eval-cost tcx cln))))


(define (check-expr tcx exp) ; -> types
  (define (recur exp) (check-expr tcx exp))

  (cond
   ((integer? exp)
    (unless (and (>= exp #x-80000000) (<= exp #x7fffffff))
      (error "integer out of range:" exp))
    '(int))

   ((symbol? exp)
    (list (tcx-ref tcx exp)))

   ((or (null? exp) (not (list? exp)))
    (error "unrecognized expression:" exp))

   ((and (= (length exp) 2) (assq (car exp) expr-primops)) =>
    (lambda (opinfo)
      (let* ((arg (cadr exp))
	     (given (recur arg))
	     (needed (primop-in opinfo)))
	(unless (= (length given) (length needed))
	  (error "operator arity mismatch:" exp needed))
	(unless (subtypes? given needed)
	  (error "operator type mismatch:" exp needed))
	(primop-out opinfo))))

   ((and (eq? (car exp) '&))
    (apply append (map recur (cdr exp))))

   ((and (eq? (car exp) 'if) (= (length exp) 4))
     (let ((predic (cadr exp))
	   (conseq (caddr exp))
	   (altern (cadddr exp)))
       (unless (subtypes? (recur predic) '(int))
	 (error "condition guard is not an int in" exp))
       (let ((tconseq (recur conseq))
	     (taltern (recur altern)))
	 (unless (= (length tconseq) (length taltern))
	   (error "arity mismatch in conditional:" exp))
	 (types-lub tconseq taltern))))

   ((and (eq? (car exp) 'lambda) (= (length exp) 3))
    (check-namelist (cadr exp))
    (check-stmt (tcx-nest tcx (cadr exp)) (tcx-lambda-number tcx) (caddr exp))
    '(lambda))

   ((and (eq? (car exp) 'class) (= (length exp) 4))
    (let ((cln (cadr exp))
	  (frame (caddr exp)))
      (unless (type-class? cln)
	(error "invalid class name:" cln))
      (let ((atys (check-namelist frame)))
	(tcx-note-class! tcx (cadr exp) atys)
	(check-stmt (tcx-nest tcx frame) cln (cadddr exp)))
      (list cln)))

   ((and (eq? (car exp) 'set) (= (length exp) 3))
    (let* ((vars (cadr exp))
	   (init (caddr exp))
	   (vtys (map (lambda (v) (tcx-ref tcx v)) vars))
	   (itys (recur init)))
      (unless (subtypes? itys vtys)
	(error "mutation type mismatch:" vtys exp))
      '()))

   ((and (eq? (car exp) 'unsafe) (= (length exp) 2))
    (for/list ((ty (in-list (recur (cadr exp))))) '_!_))

   (else
    (error "unrecognized expression:" exp))))


(define (expr-cost exp (join? #f))
  (cond
   ((or (integer? exp)
	(symbol? exp)
	(memq (car exp) '(class lambda))) 1)
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
   ((eq? (car exp) 'unsafe)
    (expr-cost (cadr exp) join?))
   (else
    (error "internal error: unrecognized expression:" exp))))

(define (check-stmt tcx cln stmt (cost (cn+)))
  (cond
   ((or (null? stmt) (not (list? stmt)))
    (error "unrecognized statement:" stmt))

   ((and (eq? (car stmt) 'ret) (= (length stmt) 2))
    (unless (symbol? cln)
      (error "not allowed in a lambda:" stmt))
    (let ((exp (cadr stmt)))
      (tcx-note-ret! tcx cln (check-expr tcx (cadr stmt)))
      (tcx-note-cost! tcx cln (cn+ cost (expr-cost exp) 2))))

   ((and (eq? (car stmt) 'halt) (= (length stmt) 2))
    (let ((exp (cadr stmt)))
      (check-expr tcx exp)
      (tcx-note-cost! tcx cln (cn+ cost (expr-cost exp) 1))))

   ((and (eq? (car stmt) 'goto) (= (length stmt) 3))
    (unless (symbol? cln)
      (error "not allowed in a lambda:" stmt))
    (let ((ftys (check-expr tcx (cadr stmt)))
	  (etys (check-expr tcx (caddr stmt))))
      (unless (and (= (length ftys) 1) (type-class? (car ftys)))
	(error "not a class instance:" (cadr stmt)))
      (let ((callee (car ftys)))
	(tcx-note-goto! tcx cln callee etys)
	(tcx-note-cost! tcx cln (cn+ cost (expr-cost (cadr stmt)) 
				     (expr-cost (caddr stmt)) 2 callee)))))

   ((and (eq? (car stmt) 'bind) (= (length stmt) 3))
    (let-values (((new-tcx bind-cost)
		  (check-bind tcx (cadr stmt))))
      (check-stmt new-tcx cln (caddr stmt) (cn+ cost bind-cost))))

   ((and (eq? (car stmt) 'seq) (= (length stmt) 3))
    (let ((exp (cadr stmt)))
      (unless (null? (check-expr tcx exp))
	(error "non-nullary expression used for effect only:" (cadr stmt)))
      (check-stmt tcx cln (caddr stmt) (cn+ cost (expr-cost exp)))))

   ((and (eq? (car stmt) 'if) (= (length stmt) 4))
    (unless (subtypes? (check-expr tcx (cadr stmt)) '(int))
      (error "condition guard is not an int in" stmt))
    (let ((cost (cn+ cost (expr-cost (cadr stmt)) 1)))
      (check-stmt tcx cln (caddr stmt) cost)
      (check-stmt tcx cln (cadddr stmt) cost)))

   ((and (eq? (car stmt) 'declaring) (= (length stmt) 3))
    (case (caadr stmt)
      ((max-depth)
       (tcx-note-depth! tcx cln (cadadr stmt))))
    (check-stmt tcx cln (caddr stmt) cost))

   (else
    (error "unrecognized statement:" stmt))))


(define (check-bind tcx bind) ; -> tcx' cost
  (cond
   ((or (null? bind) (not (list? bind)))
    (error "unrecognized binding:" bind))

   ((and (memq (car bind) '(var rec)) (odd? (length bind)))
    (let-values
	(((rec?) (eq? (car bind) 'rec))
	 ((frame vtyss exprs)
	  (let loop ((stuff (cdr bind)) (a-decls '()) (a-vtys '()) (a-expr '()))
	    (if (null? stuff)
		(values (apply append (reverse a-decls))
			(reverse a-vtys)
			(reverse a-expr))
		(let* ((decls (car stuff))
		       (vtys (check-namelist decls))
		       (expr (cadr stuff)))
		  (loop (cddr stuff)
			(cons decls a-decls)
			(cons vtys a-vtys)
			(cons expr a-expr)))))))
      (let ((new-tcx (tcx-nest tcx frame)))
	(for ((vtys vtyss) (expr exprs))
	  (let ((etys (check-expr (if rec? new-tcx tcx) expr)))
	    (unless (subtypes? etys vtys)
	      (error "type mismatch in binding:" bind))))
	(values
	 new-tcx
	 (apply cn+ (if rec? 3 2)
		(map expr-cost exprs))))))

   ((and (eq? (car bind) 'call) (= (length bind) 4))
    (let ((vtys (check-namelist (cadr bind)))
	  (ftys (check-expr tcx (caddr bind)))
	  (etys (check-expr tcx (cadddr bind))))
      (unless (and (= (length ftys) 1) (type-class? (car ftys)))
	(error "not a class instance:" (caddr bind)))
      (tcx-note-call! tcx (car ftys) etys vtys)
      (values
       (tcx-nest tcx (cadr bind))
       (cn+ (expr-cost (caddr bind))
	    (expr-cost (cadddr bind))
	    2
	    (car ftys)))))

   (else
    (error "unrecognized binding:" bind))))


(define (check-toplevel tcx tl)
  (check-stmt tcx (tcx-lambda-number tcx) tl))

