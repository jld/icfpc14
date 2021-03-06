So I spent most of my time playing with language ideas for the
Lambda-Man CPU.  I started with a fairly plain Lisp (see below), but
wound up with something... stranger.  Notes about that:

* The `.rkt` files are Racket; among the regrettable code style
  choices is not using modules and just `load`ing everything.  (In
  particular, this leads to some fun where Racket's actual macro
  expander's `expand` sometimes fails to be shadowed by my `expand`
  and I have to `load` the file again.)

* The basic pipeline is an expander for derived forms in `expndr.rkt`,
  a syntax checker / type checker / time bound estimator in
  `ncheck.rkt`, and the compiler in `ncmplr.rkt`.  Code written in the
  language is in `nexamples.rkt` and `grid.rkt`, especially `gridbot0`.

* The form named `class` is basically a lambda with a name, which is
  effectively a nominal type, the meaning of which is collected via
  subtype operations from constraints imposed by functions belonging
  to it and calls to it.  (The form named `lambda` is for the FFI.)

* Specifically, in order to do a call, the callee expression must have
  a class type (which is also a supertype of `int` to allow for
  nullability); this means that it has a fixed and probably small set
  of possible functions it can go to.  This was inspired by wanting to
  do static running-time analysis that didn't break down at every
  function call -- even if that function had been fished out of an
  environment frame in the heap somewhere.

* That, and I was planning on building mutable structures -- linked
  object graphs for the map (constant-time traversal!), queues for
  BFS, that kind of thing -- and I was worried about making
  difficult-to-debug mistakes.

* Also, everything is CPS, and multiple values are everywhere and can
  be sloshed around in disturbing ways.  The other thing I'd been
  missing in the regular Lisp I implemented first was decent handling
  of multiple values and for-effect expressions (no values) -- again,
  including across function calls.  Someone on IRC mentioned that
  excess arguments can be ignored in the standard ABI, so this was
  kind of playing with that idea.

* In theory, having all arguments on the env-stack and nothing on the
  data-stack would allow generators or yielding or workers or
  something, which could interact in useful ways with the
  time-bounding.  In practice, I didn't have time to get to it.

* Also, the strange expression/statement split I wound up with, which
  seemed so reasonable when I was laying it out, turned out to be
  confusing in practice.  Needs more thought.

* While I'm mourning broken features: the time analysis almost works
  but not quite.  The data is there, but the algorithm for turning
  that into actual numbers is broken.  (Also, no story for "heavy
  branch" conditionals.)

* There are more notes in `NOTES`, but they're kind of
  stream-of-consciousness notes to myself (and occasional fragmentary
  to-do lists) and may or may not make any actual sense.

* Yes, I really	did do all my testing by piping to `xclip` and pasting
  into the web forms: `(xclip-toplevel (expand gridbot0))`.

The bot:

* Translates the list-of-lists map into a linked graph by using
  environment mutation and tracks pointers to nodes.  Does a
  depth-first search because that's all I had time to implement.  The
  big idea was to use BFS with an imperative queue and iterate until
  it ran out of CPU time.  Sadly, dropped for lack of real time.

* Completely fails to notice where the ghosts are.  Also didn't have
  time for that.  Or enough sleep.

The ghost:

* `hunt0.gha`, processed with `gpp.pl`.  The comments there explain it
  pretty well.

The old language:

* `cmplr.rkt`, which compiles a tiny Lisp dialect (mostly Scheme-like
   naming rather than CL, but Lisp-2) into code for the Lambda-Man
   CPU.  The host language is Racket (but I haven't yet bothered with
   using modules instead of just `load`ing into a REPL -- among other
   questionable code style choices).

* `examples.rkt`, which is where the currently extant AIs are.
  `simplesearch` is the best (least bad?) one I've written so far.

* `NOTES`, where I have a bunch of notes to myself about ideas I
  haven't implemented yet, and indeed may never actually implement.
