Actions running multiple programs
=================================

In the bellow example, we have an action that executes several
external programs in sequence. Several of these programs print
something to stdout.

We arange things so that while this sequence of programs are being
executed, Dune ends up running another program from another
independent action that also prints to stdout.

To make sure things happen in the right order, we busy wait on files
being created.

  $ echo '(lang dune 3.0)' > dune-project
  $ cat>dune <<EOF
  > (rule
  >  (alias default)
  >  (deps (sandbox none))
  >  (action
  >   (progn
  >    (run dune_cmd echo A)
  >    (run touch x0)
  >    (run dune_cmd wait-for-file-to-appear x1)
  >    (run dune_cmd echo B))))
  > 
  > (rule
  >  (alias default)
  >  (deps (sandbox none))
  >  (action
  >   (progn
  >    (run dune_cmd wait-for-file-to-appear x0)
  >    (run dune_cmd echo Other)
  >    (run touch x1))))
  > EOF

At the moment, the output of the two actions is interleaved, which is
not great:

  $ dune build -j2
      dune_cmd alias default
  A
      dune_cmd alias default
  Other
      dune_cmd alias default
  B
