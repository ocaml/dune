Reproducing test case for https://github.com/ocaml/dune/issues/12638.

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > (using coq 0.11)
  > EOF

  $ touch foo.v bar.v

  $ jqScript=$(mktemp)

  $ cat >$jqScript <<'EOF'
  > include "dune";
  > processes |
  > .args.process_args as $arr |
  > [range(0; $arr | length - 1) as $i |
  >   if $arr[$i] == "-w" then [$arr[$i], $arr[$i + 1]] else empty end]
  > | .[]
  > EOF

  $ printFlags() {
  > dune trace cat | jq -f $jqScript
  > }

  $ cat > dune <<EOF
  > (coq.theory
  >  (name a)
  >  (modules_flags
  >   (foo (-w -deprecated-since-8.15))
  >   (bar (-w -deprecated-since-8.16))))
  > EOF

  $ dune build foo.vo
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  $ printFlags
  [
    "-w",
    "-deprecated-since-8.15"
  ]
  [
    "-w",
    "-deprecated-native-compiler-option"
  ]

  $ dune build bar.vo
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))

  $ printFlags
  [
    "-w",
    "-deprecated-since-8.16"
  ]
  [
    "-w",
    "-deprecated-native-compiler-option"
  ]
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

  $ dune build foo.vo
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  $ printFlags
  [
    "-w",
    "-deprecated-native-compiler-option"
  ]
  $ dune build bar.vo
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  $ printFlags
  [
    "-w",
    "-deprecated-since-8.16"
  ]
  [
    "-w",
    "-deprecated-native-compiler-option"
  ]
