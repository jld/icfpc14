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

(struct tcx (env bounds) #:constructor-name make-tcx)
(struct bounds (cln al au rl ru) #:mutable #:transparent #:constructor-name make-bounds)
(define (new-bounds cln) (make-bounds cln '_!_ '() '_!_ '()))
(define (bounds-check! b)
  (unless (subtypes? (bounds-al b) (bounds-au b))
    (error 'bounds-check! "class ~a called with ~a but expects ~a"
	   (bounds-cln b) (bounds-al b) (bounds-au b)))
  (unless (subtypes? (bounds-rl b) (bounds-ru b))
    (error 'bounds-check! "class ~a returns ~a but asked for ~a"
	   (bounds-cln b) (bounds-rl b) (bounds-ru b))))

(define (new-tcx env) (make-tcx env (make-hasheq)))
(define (tcx-nest tcx frame) (make-tcx (cons frame (tcx-env tcx))
				       (tcx-bounds tcx)))
(define (tcx-ref tcx id) (env-lookup-type (tcx-env tcx) id))
(define (tcx-get-bounds tcx cln)
  (hash-ref! (tcx-bounds tcx) cln (lambda () (new-bounds cln))))

(define (bounds-note-class! b atys)
  (set-bounds-au! b (types-glb (bounds-au b) atys))
  (bounds-check! b))
(define (bounds-note-ret! b rtys)
  (set-bounds-rl! b (types-lub (bounds-rl b) rtys))
  (bounds-check! b))
(define (bounds-note-call! b atys rtys)
  (set-bounds-al! b (types-lub (bounds-al b) atys))
  (set-bounds-ru! b (types-glb (bounds-ru b) rtys))
  (bounds-check! b))

(define (tcx-note-class! tcx cln atys)
  (bounds-note-class! (tcx-get-bounds tcx cln) atys))
(define (tcx-note-ret! tcx cln rtys)
  (bounds-note-ret! (tcx-get-bounds tcx cln) rtys))
(define (tcx-note-call! tcx cln atys rtys)
  (bounds-note-call! (tcx-get-bounds tcx cln) atys rtys))
(define (tcx-note-goto! tcx fcln tcln atys)
  (let ((bf (tcx-get-bounds tcx fcln))
	(bt (tcx-get-bounds tcx tcln)))
    (bounds-note-call! bt atys (bounds-ru bf))
    (bounds-note-ret! bf (bounds-rl bt))))

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
       (types-lub (recur conseq) (recur altern))))

   ((and (eq? (car exp) 'lambda) (= (length exp) 3))
    (check-namelist (cadr exp))
    (check-stmt tcx #f (caddr exp))
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

   ((and (eq? (car exp) 'set) (list? exp) (= (length exp) 3))
    (let* ((vars (cadr exp))
	   (init (caddr exp))
	   (vtys (map (lambda (v) (tcx-ref tcx v)) vars))
	   (itys (recur init)))
      (unless (subtypes? itys vtys)
	(error "mutation type mismatch:" vtys exp))
      '()))

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
   (else
    (error "unrecognized expression:" exp))))


(define (check-stmt tcx cln stmt)
  (cond
   ((or (null? stmt) (not (list? stmt)))
    (error "unrecognized statement:" stmt))

   ((and (eq? (car stmt) 'ret) (= (length stmt) 2))
    (unless cln
      (error "not allowed in a lambda:" stmt))
    (tcx-note-ret! tcx cln (check-expr tcx (cadr stmt))))

   ((and (eq? (car stmt) 'halt) (= (length stmt) 2))
    (check-expr tcx (cadr stmt))
    (void))

   ((and (eq? (car stmt) 'goto) (= (length stmt) 3))
    (unless cln
      (error "not allowed in a lambda:" stmt))
    (let ((ftys (check-expr tcx (cadr stmt)))
	  (etys (check-expr tcx (caddr stmt))))
      (unless (and (= (length ftys) 1) (type-class? (car ftys)))
	(error "not a class instance:" (cadr stmt)))
      (tcx-note-goto! tcx cln (car ftys) etys)))

   ((and (eq? (car stmt) 'bind) (= (length stmt) 3))
    (check-stmt (check-bind tcx (cadr stmt)) (caddr stmt) cln))

   ((and (eq? (car stmt) 'seq) (= (length stmt) 3))
    (unless (null? (check-expr tcx (cadr stmt)))
      (error "non-nullary expression used for effect only:" (cadr stmt)))
    (check-stmt tcx (caddr stmt) cln))

   ((and (eq? (car stmt) 'if) (= (length stmt) 4))
    (unless (subtypes? (check-expr tcx (cadr stmt)) '(int))
      (error "condition guard is not an int in" stmt))
    (check-stmt tcx cln (caddr stmt))
    (check-stmt tcx cln (cadddr stmt)))

   (else
    (error "unrecognized statement:" stmt))))


(define (check-bind tcx bind) ; -> tcx'
  (cond
   ((or (null? bind) (not (list? bind)))
    (error "unrecognized binding:" bind))

   ((and (memq (car bind) '(var rec)) (odd? (length bind)))
    (let-values
	(((rec?) (eq? (car bind) 'rec))
	 ((frame vtys exprs)
	  (let loop ((stuff (cdr bind)) (a-decls '()) (a-vtys '()) (a-exprs '()))
	    (if (null? stuff)
		(values (apply append (reverse a-decls))
			(reverse a-vtys)
			(reverse a-exprs))
		(let* ((decls (car stuff))
		       (vtys (check-namelist decls))
		       (expr (cadr stuff)))
		  (loop (cddr stuff)
			(cons decls a-decls)
			(cons vtys a-vtys)
			(cons exprs a-exprs)))))))
      (let ((new-tcx (tcx-nest tcx frame)))
	(for ((vty vtys) (expr exprs))
	  (let ((etys (check-expr (if rec? new-tcx tcx) expr)))
	    (unless (subtypes? etys vtys)
	      (error "type mismatch in binding:" bind))))
	new-tcx)))

   ((and (eq? (car bind) 'call) (= (length bind) 4))
    (let ((vtys (check-namelist (cadr bind)))
	  (ftys (check-expr (caddr bind)))
	  (etys (check-expr (cadddr bind))))
      (unless (and (= (length ftys) 1) (type-class? (car ftys)))
	(error "not a class instance:" (caddr bind)))
      (tcx-note-call! tcx (car ftys) etys vtys)
      (tcx-nest tcx (cadr bind))))

   (else
    (error "unrecognized binding:" bind))))


(define (check-toplevel tcx tl)
  (check-stmt tcx tl #f))

