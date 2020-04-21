The new pipe actions are only available since dune 2.7:

  $ cat >dune-project <<EOF
  > (lang dune 2.6)
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (alias pipe)
  >  (action
  >   (pipe-outputs (echo "a\nb\nc") (run grep "a\\\\|b") (run grep "b\\\\|c"))))
  > EOF

  $ dune build @pipe
  File "dune", line 4, characters 2-71:
  4 |   (pipe-outputs (echo "a\nb\nc") (run grep "a\\|b") (run grep "b\\|c"))))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: 'pipe-outputs' is only available since version 2.7 of the dune
  language. Please update your dune-project file to have (lang dune 2.7).
  [1]

You need to set the language to 2.7 or higher for it to work:

  $ cat >dune-project <<EOF
  > (lang dune 2.7)
  > EOF

  $ dune build @pipe
          grep alias pipe
  b

The makefile version of pipe actious uses actual pipes.
Note that we wrap the previous into a with-stdout-to so that we can easily refer
to the target in the dune rule invocation below.

  $ cat >dune <<EOF
  > (rule
  >  (alias pipe)
  >  (action
  >   (with-outputs-to target
  >    (pipe-outputs (echo "a\nb\nc") (run grep "'a\\\\|b'") (run grep "'b\\\\|c'")))))
  > EOF

  $ dune rule -m target
  _build/default/target: /usr/bin/grep
  	mkdir -p _build/default; \
  	mkdir -p _build/default; \
  	cd _build/default; \
  	{ echo -n c; echo b; echo a; } 2>&1 | /usr/bin/grep a\|b 2>&1 | /usr/bin/grep \
  	                                                                  b\|c &> \
  	  target
  
