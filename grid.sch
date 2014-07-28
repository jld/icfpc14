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
     )
   `((var (wmap lman ghos fruit) (untuple 4 *world*))
     (var (lmvit lmloc lmdir lmliv lmsc) (untuple 5 lman))
     (var ((: new-lmx int) (: new-lmy int)) (unsafe (untuple 2 lmloc)))

     (defun find-it ((: basex int) (: basey int) (: here cell)
		     (: thatx int) (: thaty int) (: n int))
       (cond
	((and (= basex thatx) (= basey thaty))
	 (ret here))
	((null? here) (ret 0))
	((=0 n) (ret 0))
	(else
	 (call ((: up cell) (: rt cell) (: dn cell) (: lf cell)) here)
	 (call (: upthere cell) find-it basex (- basey 1) up thatx thaty (- n 1))
	 ; This should be generated.
	 (if (thing? upthere) (ret upthere)
	     (begin
	       (call (: rtthere cell) find-it (+ basex 1) basey rt thatx thaty (- n 1))
	       (if (thing? rtthere) (ret rtthere)
		   (begin
		     (call (: dnthere cell) find-it basex (+ basey 1) dn thatx thaty (- n 1))
		     (if (thing? dnthere) (ret dnthere)
			 (goto find-it (- basex 1) basey lf thatx thaty (- n 1))))))))))
     
     (when (and (= new-lmx lmx0) (= new-lmy lmy0))
       (set (lmx lmy here) lmx0 lmy0 here0))

     (call (: new-here cell) find-it lmx lmy here new-lmx new-lmy 2)
     (set (lmx lmy here) new-lmx new-lmy new-here)

     ; (debug (cons (cons lmx lmy) here))

     (call ((: up cell) (: rt cell) (: dn cell) (: lf cell) _v (: set-here cell-poke)) here)
     (call () set-here 1)
     
     (var bestdir lmdir (: bestn int) -1)

     (rec ((: dfs-up dfs) (: dfs-rt dfs) (: dfs-dn dfs) (: dfs-lf dfs))
	  (& ,@(for/list ((fn '(dfs-up dfs-rt dfs-dn dfs-lf)) (i (in-naturals)))
		 `(class dfs ((: loc cell) odir (: n int))
		    (when (and (thing? loc) n)
		      (call ((: up cell) (: rt cell) (: dn cell) (: lf cell) (: v int)) loc)
		      (var (: faken int) (* n (if (< v 4) (- v 1) 0)))
		      (if (> faken bestn)
			  (set (bestdir bestn) odir faken)
			  (&))
		      ,@(for/list ((fn2 '(dfs-up dfs-rt dfs-dn dfs-lf))
				   (no '(dfs-dn dfs-lf dfs-up dfs-rt))
				   (dir '(up rt dn lf))
				   #:unless (eq? fn no))
			  ;; Last one should be a goto.
			  ;; Sure is a shame we're not in a language where that's automatic
			  ;; anymore....  (Oops.)
			  `(call () ,fn2 ,dir odir (- n 1))))))))

     ; Wat.
     (var (: lmdir int) (unsafe lmdir))
     (when (<= lmdir 0) (call () dfs-up up 0 10))
     (when (<= lmdir 1) (call () dfs-rt rt 1 10))
     (when (<= lmdir 2) (call () dfs-dn dn 2 10))
     (when (<= lmdir 3) (call () dfs-lf lf 3 10))
     (when (> lmdir 0) (call () dfs-up up 0 10))
     (when (> lmdir 1) (call () dfs-rt rt 1 10))
     (when (> lmdir 2) (call () dfs-dn dn 2 10))
     )
   'bestdir
   ))
