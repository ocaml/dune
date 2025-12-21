Test cases to check Coq's flag setting is correct:

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > (using rocq 0.11)
  > EOF

  $ cat > foo.v <<EOF
  > Definition t := 3.
  > EOF

Test case: default flags

  $ cat > dune <<EOF
  > (rocq.theory
  >  (name foo))
  > EOF

  $ runFlags() {
  > dune clean
  > dune build foo.vo
  > dune trace cat | jq -c '.[] | select(.name == "rocq") | .args.process_args | .[] | sub(".*/coq/"; "coq/")'
  > }

  $ runFlags
  "--config"
  "dep"
  "-boot"
  "-R"
  "coq/theories"
  "Corelib"
  "-R"
  "."
  "foo"
  "-dyndep"
  "opt"
  "-vos"
  "foo.v"
  "compile"
  "-q"
  "-w"
  "-deprecated-native-compiler-option"
  "-w"
  "-native-compiler-disabled"
  "-native-compiler"
  "ondemand"
  "-boot"
  "-R"
  "coq/theories"
  "Corelib"
  "-R"
  "."
  "foo"
  "foo.v"

TC: :standard

  $ cat > dune <<EOF
  > (rocq.theory
  >  (name foo)
  >  (flags :standard))
  > EOF

  $ runFlags
  "--config"
  "dep"
  "-boot"
  "-R"
  "coq/theories"
  "Corelib"
  "-R"
  "."
  "foo"
  "-dyndep"
  "opt"
  "-vos"
  "foo.v"
  "compile"
  "-q"
  "-w"
  "-deprecated-native-compiler-option"
  "-w"
  "-native-compiler-disabled"
  "-native-compiler"
  "ondemand"
  "-boot"
  "-R"
  "coq/theories"
  "Corelib"
  "-R"
  "."
  "foo"
  "foo.v"

TC: override :standard

  $ cat > dune <<EOF
  > (rocq.theory
  >  (name foo)
  >  (flags ))
  > EOF

  $ runFlags
  "--config"
  "dep"
  "-boot"
  "-R"
  "coq/theories"
  "Corelib"
  "-R"
  "."
  "foo"
  "-dyndep"
  "opt"
  "-vos"
  "foo.v"
  "compile"
  "-w"
  "-deprecated-native-compiler-option"
  "-w"
  "-native-compiler-disabled"
  "-native-compiler"
  "ondemand"
  "-boot"
  "-R"
  "coq/theories"
  "Corelib"
  "-R"
  "."
  "foo"
  "foo.v"

TC: add to :standard

  $ cat > dune <<EOF
  > (rocq.theory
  >  (name foo)
  >  (flags :standard -type-in-type))
  > EOF

  $ runFlags
  "--config"
  "dep"
  "-boot"
  "-R"
  "coq/theories"
  "Corelib"
  "-R"
  "."
  "foo"
  "-dyndep"
  "opt"
  "-vos"
  "foo.v"
  "compile"
  "-q"
  "-type-in-type"
  "-w"
  "-deprecated-native-compiler-option"
  "-w"
  "-native-compiler-disabled"
  "-native-compiler"
  "ondemand"
  "-boot"
  "-R"
  "coq/theories"
  "Corelib"
  "-R"
  "."
  "foo"
  "foo.v"

TC: extend in workspace + override standard

  $ cat > dune <<EOF
  > (rocq.theory
  >  (name foo)
  >  (flags :standard))
  > EOF

  $ cat > dune-workspace <<EOF
  > (lang dune 3.21)
  > (env (dev (rocq (flags -type-in-type))))
  > EOF

  $ runFlags
  "--config"
  "dep"
  "-boot"
  "-R"
  "coq/theories"
  "Corelib"
  "-R"
  "."
  "foo"
  "-dyndep"
  "opt"
  "-vos"
  "foo.v"
  "compile"
  "-type-in-type"
  "-w"
  "-deprecated-native-compiler-option"
  "-w"
  "-native-compiler-disabled"
  "-native-compiler"
  "ondemand"
  "-boot"
  "-R"
  "coq/theories"
  "Corelib"
  "-R"
  "."
  "foo"
  "foo.v"

TC: extend in workspace + override standard

  $ cat > dune-workspace <<EOF
  > (lang dune 3.21)
  > (env (dev (rocq (flags :standard -type-in-type))))
  > EOF

  $ runFlags
  "--config"
  "dep"
  "-boot"
  "-R"
  "coq/theories"
  "Corelib"
  "-R"
  "."
  "foo"
  "-dyndep"
  "opt"
  "-vos"
  "foo.v"
  "compile"
  "-q"
  "-type-in-type"
  "-w"
  "-deprecated-native-compiler-option"
  "-w"
  "-native-compiler-disabled"
  "-native-compiler"
  "ondemand"
  "-boot"
  "-R"
  "coq/theories"
  "Corelib"
  "-R"
  "."
  "foo"
  "foo.v"

TC: extend in dune (env) + override standard

  $ cat > dune <<EOF
  > (rocq.theory
  >  (name foo))
  > (env (dev (rocq (flags -type-in-type))))
  > EOF

  $ runFlags
  "--config"
  "dep"
  "-boot"
  "-R"
  "coq/theories"
  "Corelib"
  "-R"
  "."
  "foo"
  "-dyndep"
  "opt"
  "-vos"
  "foo.v"
  "compile"
  "-type-in-type"
  "-w"
  "-deprecated-native-compiler-option"
  "-w"
  "-native-compiler-disabled"
  "-native-compiler"
  "ondemand"
  "-boot"
  "-R"
  "coq/theories"
  "Corelib"
  "-R"
  "."
  "foo"
  "foo.v"

TC: extend in dune (env) + standard

  $ cat > dune <<EOF
  > (rocq.theory
  >  (name foo))
  > (env (dev (rocq (flags :standard -type-in-type))))
  > EOF

  $ runFlags
  "--config"
  "dep"
  "-boot"
  "-R"
  "coq/theories"
  "Corelib"
  "-R"
  "."
  "foo"
  "-dyndep"
  "opt"
  "-vos"
  "foo.v"
  "compile"
  "-q"
  "-type-in-type"
  "-type-in-type"
  "-w"
  "-deprecated-native-compiler-option"
  "-w"
  "-native-compiler-disabled"
  "-native-compiler"
  "ondemand"
  "-boot"
  "-R"
  "coq/theories"
  "Corelib"
  "-R"
  "."
  "foo"
  "foo.v"

TC: extend in dune (env) + workspace + standard

  $ cat > dune <<EOF
  > (rocq.theory
  >  (name foo))
  > (env (dev (rocq (flags :standard -bt))))
  > EOF

  $ cat > dune-workspace <<EOF
  > (lang dune 3.21)
  > (env (dev (rocq (flags :standard -type-in-type))))
  > EOF

  $ runFlags
  "--config"
  "dep"
  "-boot"
  "-R"
  "coq/theories"
  "Corelib"
  "-R"
  "."
  "foo"
  "-dyndep"
  "opt"
  "-vos"
  "foo.v"
  "compile"
  "-q"
  "-type-in-type"
  "-bt"
  "-w"
  "-deprecated-native-compiler-option"
  "-w"
  "-native-compiler-disabled"
  "-native-compiler"
  "ondemand"
  "-boot"
  "-R"
  "coq/theories"
  "Corelib"
  "-R"
  "."
  "foo"
  "foo.v"
