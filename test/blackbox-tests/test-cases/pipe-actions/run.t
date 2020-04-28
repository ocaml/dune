The new pipe actions are only available since dune 2.7:

  $ cat >dune-project <<EOF
  > (lang dune 2.6)
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (alias pipe)
  >  (action
  >   (pipe-outputs
  >    (echo "a\nb\nc")
  >    (run tr a x)
  >    (run tr b y)
  >    (run tr c z))))
  > EOF

  $ dune build @pipe
  File "dune", line 4, characters 2-84:
  4 |   (pipe-outputs
  5 |    (echo "a\nb\nc")
  6 |    (run tr a x)
  7 |    (run tr b y)
  8 |    (run tr c z))))
  Error: 'pipe-outputs' is only available since version 2.7 of the dune
  language. Please update your dune-project file to have (lang dune 2.7).
  [1]

You need to set the language to 2.7 or higher for it to work:

  $ cat >dune-project <<EOF
  > (lang dune 2.7)
  > EOF

  $ dune build @pipe
            tr alias pipe
  x
  y
  z

The makefile version of pipe actious uses actual pipes:

  $ touch a.ml b.ml c.ml dummy.opam
  $ cat >dune <<EOF
  > (executables
  >  (public_names a b c))
  > 
  > (rule
  >  (alias pipe)
  >  (action
  >   (with-outputs-to target
  >    (pipe-outputs (run a) (run b) (run c)))))
  > EOF

  $ dune rule -m target
  _build/default/target: _build/install/default/bin/a \
    _build/install/default/bin/b _build/install/default/bin/c
  	mkdir -p _build/default; \
  	mkdir -p _build/default; \
  	cd _build/default; \
  	../install/default/bin/a 2>&1 | ../install/default/bin/b 2>&1 | ../install/default/bin/c \
  	  &> target
  
