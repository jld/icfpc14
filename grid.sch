(define 8bit '(declare (max-depth 256)))

(define (make-cell init-val)
  `(begin
     (var ((: up cell) (: rt cell) (: dn cell) (: lf cell)) (& 0 0 0 0)
	  (: val int) ,init-val)
     (ret
      (class cell ()
	(ret up rt dn lf val (class cell-poke (: nval int) (set val nval))))
      (class cell-link ((: dir int) (: ptr cell))
	(if (< dir 2)
	    (if (=0 dir)
		(set up ptr)
		(set rt ptr))
	    (if (= 2 dir)
		(set dn ptr)
		(set lf ptr)))))))

(define dir-up 0)
(define dir-rt 1)
(define dir-dn 2)
(define dir-lf 3)

(define (stateless inits mains dir)
  `(begin
     ,@inits
     (halt (cons 0 (lambda (,(gensym 'ignored-state) *world*)
		     ,@mains (halt (cons 0 ,dir)))))))

(define gridbot0
  (stateless
   `((var (wmap0 lman0 ghos0 fruit0) (untuple 4 *initial-world*))
     (var (lmvit0 lmloc0 lmdir0 lmliv0 lmsc0) (untuple 5 lman0))
     (var (: here0 cell) 0)
     (var ((: lmx0 int) (: lmy0 int)) (unsafe (untuple 2 lmloc0)))

     (defun revappend (l1 l2) ,8bit
       (if (null? l1) (ret l2)
	   (goto revappend (cdr l1) (cons (car l1) l2))))

     (defun handle-line ((: x int) (: y int)
			 thisright (: left-cell cell) (: left-link cell-link)
			 upright-xcells thisleft-xcells)
       ,8bit
       (if (null? thisright) (goto revappend thisleft-xcells 0)
	   (begin
	     (block ((: this-cell cell) (: this-link cell-link))
		    (var (: cellval int) (unsafe (car thisright)))
		    (if cellval
			,(make-cell 'cellval)
			(ret 0 0)))
	     (if (= x lmx0) (if (= y lmy0) (set here0 this-cell) (&)) (&))
	     (when (thing? this-cell)
	       (when (thing? left-cell)
		 (call () this-link ,dir-lf left-cell)
		 (call () left-link ,dir-rt this-cell))
	       (var ((: up-cell cell) (: up-link cell-link))
		    (if (null? upright-xcells) (& 0 0)
			(unsafe (& (car (car upright-xcells))
				   (cdr (car upright-xcells))))))
	       (when (thing? up-cell)
		 (call () this-link ,dir-up up-cell)
		 (call () up-link ,dir-dn this-cell)))
	     (goto handle-line (+ x 1) y
		   (cdr thisright) this-cell this-link
		   (if (null? upright-xcells) 0 (cdr upright-xcells))
		   (cons (cons this-cell this-link) thisleft-xcells)))))

     (defun handle-block ((: y int) thisdown up-xcells)
       ,8bit
       (unless (null? thisdown)
	 (call this-xcells handle-line 0 y (car thisdown) 0 0 up-xcells 0)
	 (goto handle-block (+ y 1) (cdr thisdown) this-xcells)))

     (call () handle-block 0 wmap0 0)

     (var (: lmx int) lmx0 (: lmy int) lmy0 (: here cell) here0)
     (var (: olmx int) lmx (: olmy int) lmy (: ohere cell) here)
     )
   '((var (wmap lman ghos fruit) (untuple 4 *world*))
     (var (lmvit lmloc lmdir lmliv lmsc) (untuple 5 lman))
     (var ((: new-lmx int) (: new-lmy int)) (unsafe (untuple 2 lmloc)))

     (cond
      ((and (= new-lmx lmx) (= new-lmy lmy)))
      ((and (= new-lmx olmx) (= new-lmy olmy))
       (set lmx olmx) (set lmy olmy) (set here ohere))
      ((and (= new-lmx lmx0) (= new-lmy lmy0))
       (set lmx lmx0) (set lmy lmy0) (set here here0))
      (else (debug #xDEADBEE)))

     (set (olmx olmy ohere) lmx lmy here)
     (call (hup hrt hdn (: hlf cell) val (: setval cell-poke)) here)
     (call () setval 1)
     (if (thing? hlf)
	 (set (lmx here) (- lmx 1) hlf)
	 (&))
     )
   3
   ))
