Test cases to check Coq's flag setting is correct:

  $ cat > dune-project <<EOF
  > (lang dune 2.8)
  > (using coq 0.3)
  > EOF

  $ cat > foo.v <<EOF
  > Definition t := 3.
  > EOF

Test case: default flags

  $ cat > dune <<EOF
  > (coq.theory
  >  (name foo))
  > EOF

  $ runFlags() {
  > dune trace cat | jq 'include "dune"; rocqFlags'
  > }

  $ dune build foo.vo
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  $ runFlags
  {
    "name": "coqc",
    "args": [
      "--config"
    ]
  }
  {
    "name": "coqdep",
    "args": [
      "-I",
      "coq-core/plugins/btauto",
      "-I",
      "coq-core/plugins/cc",
      "-I",
      "coq-core/plugins/derive",
      "-I",
      "coq-core/plugins/extraction",
      "-I",
      "coq-core/plugins/firstorder",
      "-I",
      "coq-core/plugins/funind",
      "-I",
      "coq-core/plugins/ltac",
      "-I",
      "coq-core/plugins/ltac2",
      "-I",
      "coq-core/plugins/micromega",
      "-I",
      "coq-core/plugins/nsatz",
      "-I",
      "coq-core/plugins/number_string_notation",
      "-I",
      "coq-core/plugins/ring",
      "-I",
      "coq-core/plugins/rtauto",
      "-I",
      "coq-core/plugins/ssreflect",
      "-I",
      "coq-core/plugins/ssrmatching",
      "-I",
      "coq-core/plugins/tauto",
      "-I",
      "coq-core/plugins/tutorial/p0",
      "-I",
      "coq-core/plugins/tutorial/p1",
      "-I",
      "coq-core/plugins/tutorial/p2",
      "-I",
      "coq-core/plugins/tutorial/p3",
      "-I",
      "coq-core/plugins/zify",
      "-R",
      "coq/theories",
      "Coq",
      "-R",
      ".",
      "foo",
      "-dyndep",
      "opt",
      "-vos",
      "foo.v"
    ]
  }
  {
    "name": "coqc",
    "args": [
      "-q",
      "-w",
      "-deprecated-native-compiler-option",
      "-w",
      "-native-compiler-disabled",
      "-native-compiler",
      "ondemand",
      "-I",
      "coq-core/plugins/btauto",
      "-I",
      "coq-core/plugins/cc",
      "-I",
      "coq-core/plugins/derive",
      "-I",
      "coq-core/plugins/extraction",
      "-I",
      "coq-core/plugins/firstorder",
      "-I",
      "coq-core/plugins/funind",
      "-I",
      "coq-core/plugins/ltac",
      "-I",
      "coq-core/plugins/ltac2",
      "-I",
      "coq-core/plugins/micromega",
      "-I",
      "coq-core/plugins/nsatz",
      "-I",
      "coq-core/plugins/number_string_notation",
      "-I",
      "coq-core/plugins/ring",
      "-I",
      "coq-core/plugins/rtauto",
      "-I",
      "coq-core/plugins/ssreflect",
      "-I",
      "coq-core/plugins/ssrmatching",
      "-I",
      "coq-core/plugins/tauto",
      "-I",
      "coq-core/plugins/tutorial/p0",
      "-I",
      "coq-core/plugins/tutorial/p1",
      "-I",
      "coq-core/plugins/tutorial/p2",
      "-I",
      "coq-core/plugins/tutorial/p3",
      "-I",
      "coq-core/plugins/zify",
      "-R",
      "coq/theories",
      "Coq",
      "-R",
      ".",
      "foo",
      "foo.v"
    ]
  }

TC: :standard

  $ cat > dune <<EOF
  > (coq.theory
  >  (name foo)
  >  (flags :standard))
  > EOF

  $ rm _build/default/foo.vo
  $ dune build foo.vo
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  $ runFlags
  {
    "name": "coqc",
    "args": [
      "--config"
    ]
  }
  {
    "name": "coqc",
    "args": [
      "-q",
      "-w",
      "-deprecated-native-compiler-option",
      "-w",
      "-native-compiler-disabled",
      "-native-compiler",
      "ondemand",
      "-I",
      "coq-core/plugins/btauto",
      "-I",
      "coq-core/plugins/cc",
      "-I",
      "coq-core/plugins/derive",
      "-I",
      "coq-core/plugins/extraction",
      "-I",
      "coq-core/plugins/firstorder",
      "-I",
      "coq-core/plugins/funind",
      "-I",
      "coq-core/plugins/ltac",
      "-I",
      "coq-core/plugins/ltac2",
      "-I",
      "coq-core/plugins/micromega",
      "-I",
      "coq-core/plugins/nsatz",
      "-I",
      "coq-core/plugins/number_string_notation",
      "-I",
      "coq-core/plugins/ring",
      "-I",
      "coq-core/plugins/rtauto",
      "-I",
      "coq-core/plugins/ssreflect",
      "-I",
      "coq-core/plugins/ssrmatching",
      "-I",
      "coq-core/plugins/tauto",
      "-I",
      "coq-core/plugins/tutorial/p0",
      "-I",
      "coq-core/plugins/tutorial/p1",
      "-I",
      "coq-core/plugins/tutorial/p2",
      "-I",
      "coq-core/plugins/tutorial/p3",
      "-I",
      "coq-core/plugins/zify",
      "-R",
      "coq/theories",
      "Coq",
      "-R",
      ".",
      "foo",
      "foo.v"
    ]
  }

TC: override :standard

  $ cat > dune <<EOF
  > (coq.theory
  >  (name foo)
  >  (flags ))
  > EOF

  $ dune build foo.vo
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  $ runFlags
  {
    "name": "coqc",
    "args": [
      "--config"
    ]
  }
  {
    "name": "coqc",
    "args": [
      "-w",
      "-deprecated-native-compiler-option",
      "-w",
      "-native-compiler-disabled",
      "-native-compiler",
      "ondemand",
      "-I",
      "coq-core/plugins/btauto",
      "-I",
      "coq-core/plugins/cc",
      "-I",
      "coq-core/plugins/derive",
      "-I",
      "coq-core/plugins/extraction",
      "-I",
      "coq-core/plugins/firstorder",
      "-I",
      "coq-core/plugins/funind",
      "-I",
      "coq-core/plugins/ltac",
      "-I",
      "coq-core/plugins/ltac2",
      "-I",
      "coq-core/plugins/micromega",
      "-I",
      "coq-core/plugins/nsatz",
      "-I",
      "coq-core/plugins/number_string_notation",
      "-I",
      "coq-core/plugins/ring",
      "-I",
      "coq-core/plugins/rtauto",
      "-I",
      "coq-core/plugins/ssreflect",
      "-I",
      "coq-core/plugins/ssrmatching",
      "-I",
      "coq-core/plugins/tauto",
      "-I",
      "coq-core/plugins/tutorial/p0",
      "-I",
      "coq-core/plugins/tutorial/p1",
      "-I",
      "coq-core/plugins/tutorial/p2",
      "-I",
      "coq-core/plugins/tutorial/p3",
      "-I",
      "coq-core/plugins/zify",
      "-R",
      "coq/theories",
      "Coq",
      "-R",
      ".",
      "foo",
      "foo.v"
    ]
  }

TC: add to :standard

  $ cat > dune <<EOF
  > (coq.theory
  >  (name foo)
  >  (flags :standard -type-in-type))
  > EOF

  $ dune build foo.vo
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  $ runFlags
  {
    "name": "coqc",
    "args": [
      "--config"
    ]
  }
  {
    "name": "coqc",
    "args": [
      "-q",
      "-type-in-type",
      "-w",
      "-deprecated-native-compiler-option",
      "-w",
      "-native-compiler-disabled",
      "-native-compiler",
      "ondemand",
      "-I",
      "coq-core/plugins/btauto",
      "-I",
      "coq-core/plugins/cc",
      "-I",
      "coq-core/plugins/derive",
      "-I",
      "coq-core/plugins/extraction",
      "-I",
      "coq-core/plugins/firstorder",
      "-I",
      "coq-core/plugins/funind",
      "-I",
      "coq-core/plugins/ltac",
      "-I",
      "coq-core/plugins/ltac2",
      "-I",
      "coq-core/plugins/micromega",
      "-I",
      "coq-core/plugins/nsatz",
      "-I",
      "coq-core/plugins/number_string_notation",
      "-I",
      "coq-core/plugins/ring",
      "-I",
      "coq-core/plugins/rtauto",
      "-I",
      "coq-core/plugins/ssreflect",
      "-I",
      "coq-core/plugins/ssrmatching",
      "-I",
      "coq-core/plugins/tauto",
      "-I",
      "coq-core/plugins/tutorial/p0",
      "-I",
      "coq-core/plugins/tutorial/p1",
      "-I",
      "coq-core/plugins/tutorial/p2",
      "-I",
      "coq-core/plugins/tutorial/p3",
      "-I",
      "coq-core/plugins/zify",
      "-R",
      "coq/theories",
      "Coq",
      "-R",
      ".",
      "foo",
      "foo.v"
    ]
  }


TC: extend in workspace + override standard

  $ cat > dune <<EOF
  > (coq.theory
  >  (name foo)
  >  (flags :standard))
  > EOF

  $ cat > dune-workspace <<EOF
  > (lang dune 2.8)
  > (env (dev (coq (flags -type-in-type))))
  > EOF

  $ dune build foo.vo
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  $ runFlags
  {
    "name": "coqc",
    "args": [
      "--config"
    ]
  }
  {
    "name": "coqc",
    "args": [
      "-type-in-type",
      "-w",
      "-deprecated-native-compiler-option",
      "-w",
      "-native-compiler-disabled",
      "-native-compiler",
      "ondemand",
      "-I",
      "coq-core/plugins/btauto",
      "-I",
      "coq-core/plugins/cc",
      "-I",
      "coq-core/plugins/derive",
      "-I",
      "coq-core/plugins/extraction",
      "-I",
      "coq-core/plugins/firstorder",
      "-I",
      "coq-core/plugins/funind",
      "-I",
      "coq-core/plugins/ltac",
      "-I",
      "coq-core/plugins/ltac2",
      "-I",
      "coq-core/plugins/micromega",
      "-I",
      "coq-core/plugins/nsatz",
      "-I",
      "coq-core/plugins/number_string_notation",
      "-I",
      "coq-core/plugins/ring",
      "-I",
      "coq-core/plugins/rtauto",
      "-I",
      "coq-core/plugins/ssreflect",
      "-I",
      "coq-core/plugins/ssrmatching",
      "-I",
      "coq-core/plugins/tauto",
      "-I",
      "coq-core/plugins/tutorial/p0",
      "-I",
      "coq-core/plugins/tutorial/p1",
      "-I",
      "coq-core/plugins/tutorial/p2",
      "-I",
      "coq-core/plugins/tutorial/p3",
      "-I",
      "coq-core/plugins/zify",
      "-R",
      "coq/theories",
      "Coq",
      "-R",
      ".",
      "foo",
      "foo.v"
    ]
  }

TC: extend in workspace + override standard

  $ cat > dune-workspace <<EOF
  > (lang dune 2.8)
  > (env (dev (coq (flags :standard -type-in-type))))
  > EOF

  $ dune build foo.vo
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  $ runFlags
  {
    "name": "coqc",
    "args": [
      "--config"
    ]
  }
  {
    "name": "coqc",
    "args": [
      "-q",
      "-type-in-type",
      "-w",
      "-deprecated-native-compiler-option",
      "-w",
      "-native-compiler-disabled",
      "-native-compiler",
      "ondemand",
      "-I",
      "coq-core/plugins/btauto",
      "-I",
      "coq-core/plugins/cc",
      "-I",
      "coq-core/plugins/derive",
      "-I",
      "coq-core/plugins/extraction",
      "-I",
      "coq-core/plugins/firstorder",
      "-I",
      "coq-core/plugins/funind",
      "-I",
      "coq-core/plugins/ltac",
      "-I",
      "coq-core/plugins/ltac2",
      "-I",
      "coq-core/plugins/micromega",
      "-I",
      "coq-core/plugins/nsatz",
      "-I",
      "coq-core/plugins/number_string_notation",
      "-I",
      "coq-core/plugins/ring",
      "-I",
      "coq-core/plugins/rtauto",
      "-I",
      "coq-core/plugins/ssreflect",
      "-I",
      "coq-core/plugins/ssrmatching",
      "-I",
      "coq-core/plugins/tauto",
      "-I",
      "coq-core/plugins/tutorial/p0",
      "-I",
      "coq-core/plugins/tutorial/p1",
      "-I",
      "coq-core/plugins/tutorial/p2",
      "-I",
      "coq-core/plugins/tutorial/p3",
      "-I",
      "coq-core/plugins/zify",
      "-R",
      "coq/theories",
      "Coq",
      "-R",
      ".",
      "foo",
      "foo.v"
    ]
  }

TC: extend in dune (env) + override standard

  $ cat > dune <<EOF
  > (coq.theory
  >  (name foo))
  > (env (dev (coq (flags -type-in-type))))
  > EOF

  $ rm -rf _build/default/foo.vo
  $ dune build foo.vo
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  $ runFlags
  {
    "name": "coqc",
    "args": [
      "--config"
    ]
  }
  {
    "name": "coqc",
    "args": [
      "-type-in-type",
      "-w",
      "-deprecated-native-compiler-option",
      "-w",
      "-native-compiler-disabled",
      "-native-compiler",
      "ondemand",
      "-I",
      "coq-core/plugins/btauto",
      "-I",
      "coq-core/plugins/cc",
      "-I",
      "coq-core/plugins/derive",
      "-I",
      "coq-core/plugins/extraction",
      "-I",
      "coq-core/plugins/firstorder",
      "-I",
      "coq-core/plugins/funind",
      "-I",
      "coq-core/plugins/ltac",
      "-I",
      "coq-core/plugins/ltac2",
      "-I",
      "coq-core/plugins/micromega",
      "-I",
      "coq-core/plugins/nsatz",
      "-I",
      "coq-core/plugins/number_string_notation",
      "-I",
      "coq-core/plugins/ring",
      "-I",
      "coq-core/plugins/rtauto",
      "-I",
      "coq-core/plugins/ssreflect",
      "-I",
      "coq-core/plugins/ssrmatching",
      "-I",
      "coq-core/plugins/tauto",
      "-I",
      "coq-core/plugins/tutorial/p0",
      "-I",
      "coq-core/plugins/tutorial/p1",
      "-I",
      "coq-core/plugins/tutorial/p2",
      "-I",
      "coq-core/plugins/tutorial/p3",
      "-I",
      "coq-core/plugins/zify",
      "-R",
      "coq/theories",
      "Coq",
      "-R",
      ".",
      "foo",
      "foo.v"
    ]
  }

TC: extend in dune (env) + standard

  $ cat > dune <<EOF
  > (coq.theory
  >  (name foo))
  > (env (dev (coq (flags :standard -type-in-type))))
  > EOF

  $ rm -rf _build/default/foo.vo
  $ dune build foo.vo
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  $ runFlags
  {
    "name": "coqc",
    "args": [
      "--config"
    ]
  }
  {
    "name": "coqc",
    "args": [
      "-q",
      "-type-in-type",
      "-type-in-type",
      "-w",
      "-deprecated-native-compiler-option",
      "-w",
      "-native-compiler-disabled",
      "-native-compiler",
      "ondemand",
      "-I",
      "coq-core/plugins/btauto",
      "-I",
      "coq-core/plugins/cc",
      "-I",
      "coq-core/plugins/derive",
      "-I",
      "coq-core/plugins/extraction",
      "-I",
      "coq-core/plugins/firstorder",
      "-I",
      "coq-core/plugins/funind",
      "-I",
      "coq-core/plugins/ltac",
      "-I",
      "coq-core/plugins/ltac2",
      "-I",
      "coq-core/plugins/micromega",
      "-I",
      "coq-core/plugins/nsatz",
      "-I",
      "coq-core/plugins/number_string_notation",
      "-I",
      "coq-core/plugins/ring",
      "-I",
      "coq-core/plugins/rtauto",
      "-I",
      "coq-core/plugins/ssreflect",
      "-I",
      "coq-core/plugins/ssrmatching",
      "-I",
      "coq-core/plugins/tauto",
      "-I",
      "coq-core/plugins/tutorial/p0",
      "-I",
      "coq-core/plugins/tutorial/p1",
      "-I",
      "coq-core/plugins/tutorial/p2",
      "-I",
      "coq-core/plugins/tutorial/p3",
      "-I",
      "coq-core/plugins/zify",
      "-R",
      "coq/theories",
      "Coq",
      "-R",
      ".",
      "foo",
      "foo.v"
    ]
  }

TC: extend in dune (env) + workspace + standard

  $ cat > dune <<EOF
  > (coq.theory
  >  (name foo))
  > (env (dev (coq (flags :standard -bt))))
  > EOF

  $ cat > dune-workspace <<EOF
  > (lang dune 2.8)
  > (env (dev (coq (flags :standard -type-in-type))))
  > EOF

  $ rm -rf _build/default/foo.vo
  $ dune build foo.vo
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  $ runFlags
  {
    "name": "coqc",
    "args": [
      "--config"
    ]
  }
  {
    "name": "coqc",
    "args": [
      "-q",
      "-type-in-type",
      "-bt",
      "-w",
      "-deprecated-native-compiler-option",
      "-w",
      "-native-compiler-disabled",
      "-native-compiler",
      "ondemand",
      "-I",
      "coq-core/plugins/btauto",
      "-I",
      "coq-core/plugins/cc",
      "-I",
      "coq-core/plugins/derive",
      "-I",
      "coq-core/plugins/extraction",
      "-I",
      "coq-core/plugins/firstorder",
      "-I",
      "coq-core/plugins/funind",
      "-I",
      "coq-core/plugins/ltac",
      "-I",
      "coq-core/plugins/ltac2",
      "-I",
      "coq-core/plugins/micromega",
      "-I",
      "coq-core/plugins/nsatz",
      "-I",
      "coq-core/plugins/number_string_notation",
      "-I",
      "coq-core/plugins/ring",
      "-I",
      "coq-core/plugins/rtauto",
      "-I",
      "coq-core/plugins/ssreflect",
      "-I",
      "coq-core/plugins/ssrmatching",
      "-I",
      "coq-core/plugins/tauto",
      "-I",
      "coq-core/plugins/tutorial/p0",
      "-I",
      "coq-core/plugins/tutorial/p1",
      "-I",
      "coq-core/plugins/tutorial/p2",
      "-I",
      "coq-core/plugins/tutorial/p3",
      "-I",
      "coq-core/plugins/zify",
      "-R",
      "coq/theories",
      "Coq",
      "-R",
      ".",
      "foo",
      "foo.v"
    ]
  }
