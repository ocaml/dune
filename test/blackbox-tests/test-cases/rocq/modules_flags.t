Reproducing test case for https://github.com/ocaml/dune/issues/12638.

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > (using rocq 0.11)
  > EOF

  $ touch foo.v bar.v

  $ jqScript=$(mktemp)

  $ cat >$jqScript <<'EOF'
  > include "dune";
  > processes |
  > .args.process_args as $arr |
  > [range(0; $arr | length - 1) as $i |
  >   if ($arr[$i] == "-w" and ($arr[$i + 1] | contains("deprecated"))) then [$arr[$i], $arr[$i + 1]] else empty end]
  > | .[]
  > EOF

  $ printFlags() {
  > dune trace cat | jq -f $jqScript
  > }

  $ cat > dune <<EOF
  > (rocq.theory
  >  (name a)
  >  (modules_flags
  >   (foo (-w -deprecated-since-8.15))
  >   (bar (-w -deprecated-since-8.16))))
  > EOF

  $ dune build foo.vo
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

  $ cat > dune <<EOF
  > (rocq.theory
  >  (name a)
  >  (modules_flags
  >   (bar (-w -deprecated-since-8.16))))
  > EOF

  $ dune build foo.vo
  $ printFlags
  [
    "-w",
    "-deprecated-native-compiler-option"
  ]
  $ dune build bar.vo
  $ printFlags
  [
    "-w",
    "-deprecated-since-8.16"
  ]
  [
    "-w",
    "-deprecated-native-compiler-option"
  ]
