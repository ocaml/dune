Reproducing test case for https://github.com/ocaml/dune/issues/12638.

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > (using coq 0.11)
  > EOF

  $ touch foo.v bar.v

  $ cat > dune <<EOF
  > (coq.theory
  >  (name a)
  >  (modules_flags
  >   (foo (-w -deprecated-since-8.15))
  >   (bar (-w -deprecated-since-8.16))))
  > EOF

  $ dune build foo.vo && tail -n 1 _build/log | ./scrub_coq_args.sh | grep deprecated-since
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  -w -deprecated-since-8.15
  $ dune build bar.vo && tail -n 1 _build/log | ./scrub_coq_args.sh | grep deprecated-since
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  -w -deprecated-since-8.16
  $ dune clean

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > (using coq 0.10)
  > EOF
  $ dune build
  File "dune", line 5, characters 2-35:
  5 |   (bar (-w -deprecated-since-8.16))))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Too many arguments for "modules_flags"
  [1]
  $ dune clean

  $ cat > dune <<EOF
  > (coq.theory
  >  (name a)
  >  (modules_flags
  >   (bar (-w -deprecated-since-8.16))))
  > EOF

  $ dune build foo.vo && tail -n 1 _build/log | ./scrub_coq_args.sh | grep deprecated-since
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  [1]
  $ dune build bar.vo && tail -n 1 _build/log | ./scrub_coq_args.sh | grep deprecated-since
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  -w -deprecated-since-8.16
