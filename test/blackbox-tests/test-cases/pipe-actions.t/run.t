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
  File "dune", lines 4-8, characters 2-84:
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
  x
  y
  z

The makefile version of pipe actions uses actual pipes:

  $ touch a.ml b.ml c.ml dummy.opam
  $ cat >dune <<EOF
  > (executables
  >  (public_names a b c))
  > (rule
  >  (alias pipe)
  >  (action
  >   (with-outputs-to target
  >    (pipe-outputs (run a) (run b) (run c)))))
  > EOF

  $ dune rules -m target
  _build/default/target: _build/install/default/bin/a \
    _build/install/default/bin/b _build/install/default/bin/c
  	mkdir -p _build/default; \
  	mkdir -p _build/default; \
  	cd _build/default; \
  	../install/default/bin/a  2>&1 |  \
  	  ../install/default/bin/b | ../install/default/bin/c  &> target

  $ cat >dune <<EOF
  > (executable
  >  (public_name apl) (name append_to_line) (modules append_to_line))
  > (executable
  >  (public_name echo-outputs) (name echo_outputs) (modules echo_outputs))
  > (rule
  >  (action
  >   (with-stderr-to target-stdout.stderr
  >    (with-stdout-to target-stdout.stdout
  >     (pipe-stdout
  >      (run echo-outputs a)
  >      (run apl b)
  >      (run apl c))))))
  > (rule
  >  (action
  >   (with-stderr-to target-stderr.stderr
  >    (with-stdout-to target-stderr.stdout
  >     (pipe-stderr
  >      (run echo-outputs a)
  >      (run apl b)
  >      (run apl c))))))
  > (rule
  >  (action
  >   (with-stderr-to target-outputs.stderr
  >   (with-stdout-to target-outputs.stdout
  >    (pipe-outputs
  >     (run echo-outputs a)
  >     (run apl b)
  >     (run apl c))))))
  > EOF

  $ dune build _build/default/target-stdout.stdout _build/default/target-stdout.stderr
  $ cat _build/default/target-stdout.stdout
  o a | o b | o c
  $ cat _build/default/target-stdout.stderr
  e a
  o a | e b
  o a | o b | e c
  $ dune build _build/default/target-stderr.stdout _build/default/target-stderr.stderr
  $ cat _build/default/target-stderr.stdout
  o a
  $ cat _build/default/target-stderr.stderr
  e a | e b
  e a | o b | o c
  e a | o b | e c
  $ dune build _build/default/target-outputs.stdout _build/default/target-outputs.stderr
  $ cat _build/default/target-outputs.stdout
  o a | o b | o c
  e a | o b | o c
  $ cat _build/default/target-outputs.stderr
  o a | e b
  e a | e b
  o a | o b | e c
  e a | o b | e c
