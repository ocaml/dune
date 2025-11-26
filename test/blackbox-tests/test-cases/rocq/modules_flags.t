Reproducing test case for https://github.com/ocaml/dune/issues/12638.

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > (using rocq 0.11)
  > EOF

  $ touch foo.v bar.v

  $ cat > dune <<EOF
  > (rocq.theory
  >  (name a)
  >  (modules_flags
  >   (foo (-w -deprecated-since-8.15))
  >   (bar (-w -deprecated-since-8.16))))
  > EOF

  $ dune build foo.vo && tail -n 1 _build/log | ./scrub_coq_args.sh | grep deprecated-since
  -w -deprecated-since-8.15
  $ dune build bar.vo && tail -n 1 _build/log | ./scrub_coq_args.sh | grep deprecated-since
  -w -deprecated-since-8.16
  $ dune clean

  $ cat > dune <<EOF
  > (rocq.theory
  >  (name a)
  >  (modules_flags
  >   (bar (-w -deprecated-since-8.16))))
  > EOF

  $ dune build foo.vo && tail -n 1 _build/log | ./scrub_coq_args.sh | grep deprecated-since
  [1]
  $ dune build bar.vo && tail -n 1 _build/log | ./scrub_coq_args.sh | grep deprecated-since
  -w -deprecated-since-8.16
