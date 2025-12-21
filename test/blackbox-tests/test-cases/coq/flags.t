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
  > jq '.[] | select(.name == "coqc") | .args.process_args | .[] | sub(".*/coq/"; "coq/")' trace.json
  > }

  $ dune build foo.vo --trace-file trace.json
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  $ runFlags
  "--config"
  "-q"
  "-w"
  "-deprecated-native-compiler-option"
  "-w"
  "-native-compiler-disabled"
  "-native-compiler"
  "ondemand"
  "-I"
  "coq/../coq-core/plugins/btauto"
  "-I"
  "coq/../coq-core/plugins/cc"
  "-I"
  "coq/../coq-core/plugins/derive"
  "-I"
  "coq/../coq-core/plugins/extraction"
  "-I"
  "coq/../coq-core/plugins/firstorder"
  "-I"
  "coq/../coq-core/plugins/funind"
  "-I"
  "coq/../coq-core/plugins/ltac"
  "-I"
  "coq/../coq-core/plugins/ltac2"
  "-I"
  "coq/../coq-core/plugins/micromega"
  "-I"
  "coq/../coq-core/plugins/nsatz"
  "-I"
  "coq/../coq-core/plugins/number_string_notation"
  "-I"
  "coq/../coq-core/plugins/ring"
  "-I"
  "coq/../coq-core/plugins/rtauto"
  "-I"
  "coq/../coq-core/plugins/ssreflect"
  "-I"
  "coq/../coq-core/plugins/ssrmatching"
  "-I"
  "coq/../coq-core/plugins/tauto"
  "-I"
  "coq/../coq-core/plugins/tutorial/p0"
  "-I"
  "coq/../coq-core/plugins/tutorial/p1"
  "-I"
  "coq/../coq-core/plugins/tutorial/p2"
  "-I"
  "coq/../coq-core/plugins/tutorial/p3"
  "-I"
  "coq/../coq-core/plugins/zify"
  "-R"
  "coq/theories"
  "Coq"
  "-R"
  "."
  "foo"
  "foo.v"

TC: :standard

  $ cat > dune <<EOF
  > (coq.theory
  >  (name foo)
  >  (flags :standard))
  > EOF

  $ rm _build/default/foo.vo
  $ dune build --trace-file trace.json foo.vo
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  $ runFlags
  "--config"
  "-q"
  "-w"
  "-deprecated-native-compiler-option"
  "-w"
  "-native-compiler-disabled"
  "-native-compiler"
  "ondemand"
  "-I"
  "coq/../coq-core/plugins/btauto"
  "-I"
  "coq/../coq-core/plugins/cc"
  "-I"
  "coq/../coq-core/plugins/derive"
  "-I"
  "coq/../coq-core/plugins/extraction"
  "-I"
  "coq/../coq-core/plugins/firstorder"
  "-I"
  "coq/../coq-core/plugins/funind"
  "-I"
  "coq/../coq-core/plugins/ltac"
  "-I"
  "coq/../coq-core/plugins/ltac2"
  "-I"
  "coq/../coq-core/plugins/micromega"
  "-I"
  "coq/../coq-core/plugins/nsatz"
  "-I"
  "coq/../coq-core/plugins/number_string_notation"
  "-I"
  "coq/../coq-core/plugins/ring"
  "-I"
  "coq/../coq-core/plugins/rtauto"
  "-I"
  "coq/../coq-core/plugins/ssreflect"
  "-I"
  "coq/../coq-core/plugins/ssrmatching"
  "-I"
  "coq/../coq-core/plugins/tauto"
  "-I"
  "coq/../coq-core/plugins/tutorial/p0"
  "-I"
  "coq/../coq-core/plugins/tutorial/p1"
  "-I"
  "coq/../coq-core/plugins/tutorial/p2"
  "-I"
  "coq/../coq-core/plugins/tutorial/p3"
  "-I"
  "coq/../coq-core/plugins/zify"
  "-R"
  "coq/theories"
  "Coq"
  "-R"
  "."
  "foo"
  "foo.v"

TC: override :standard

  $ cat > dune <<EOF
  > (coq.theory
  >  (name foo)
  >  (flags ))
  > EOF

  $ dune build --trace-file trace.json foo.vo
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  $ runFlags
  "--config"
  "-w"
  "-deprecated-native-compiler-option"
  "-w"
  "-native-compiler-disabled"
  "-native-compiler"
  "ondemand"
  "-I"
  "coq/../coq-core/plugins/btauto"
  "-I"
  "coq/../coq-core/plugins/cc"
  "-I"
  "coq/../coq-core/plugins/derive"
  "-I"
  "coq/../coq-core/plugins/extraction"
  "-I"
  "coq/../coq-core/plugins/firstorder"
  "-I"
  "coq/../coq-core/plugins/funind"
  "-I"
  "coq/../coq-core/plugins/ltac"
  "-I"
  "coq/../coq-core/plugins/ltac2"
  "-I"
  "coq/../coq-core/plugins/micromega"
  "-I"
  "coq/../coq-core/plugins/nsatz"
  "-I"
  "coq/../coq-core/plugins/number_string_notation"
  "-I"
  "coq/../coq-core/plugins/ring"
  "-I"
  "coq/../coq-core/plugins/rtauto"
  "-I"
  "coq/../coq-core/plugins/ssreflect"
  "-I"
  "coq/../coq-core/plugins/ssrmatching"
  "-I"
  "coq/../coq-core/plugins/tauto"
  "-I"
  "coq/../coq-core/plugins/tutorial/p0"
  "-I"
  "coq/../coq-core/plugins/tutorial/p1"
  "-I"
  "coq/../coq-core/plugins/tutorial/p2"
  "-I"
  "coq/../coq-core/plugins/tutorial/p3"
  "-I"
  "coq/../coq-core/plugins/zify"
  "-R"
  "coq/theories"
  "Coq"
  "-R"
  "."
  "foo"
  "foo.v"

TC: add to :standard

  $ cat > dune <<EOF
  > (coq.theory
  >  (name foo)
  >  (flags :standard -type-in-type))
  > EOF

  $ dune build --trace-file trace.json foo.vo
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  $ runFlags
  "--config"
  "-q"
  "-type-in-type"
  "-w"
  "-deprecated-native-compiler-option"
  "-w"
  "-native-compiler-disabled"
  "-native-compiler"
  "ondemand"
  "-I"
  "coq/../coq-core/plugins/btauto"
  "-I"
  "coq/../coq-core/plugins/cc"
  "-I"
  "coq/../coq-core/plugins/derive"
  "-I"
  "coq/../coq-core/plugins/extraction"
  "-I"
  "coq/../coq-core/plugins/firstorder"
  "-I"
  "coq/../coq-core/plugins/funind"
  "-I"
  "coq/../coq-core/plugins/ltac"
  "-I"
  "coq/../coq-core/plugins/ltac2"
  "-I"
  "coq/../coq-core/plugins/micromega"
  "-I"
  "coq/../coq-core/plugins/nsatz"
  "-I"
  "coq/../coq-core/plugins/number_string_notation"
  "-I"
  "coq/../coq-core/plugins/ring"
  "-I"
  "coq/../coq-core/plugins/rtauto"
  "-I"
  "coq/../coq-core/plugins/ssreflect"
  "-I"
  "coq/../coq-core/plugins/ssrmatching"
  "-I"
  "coq/../coq-core/plugins/tauto"
  "-I"
  "coq/../coq-core/plugins/tutorial/p0"
  "-I"
  "coq/../coq-core/plugins/tutorial/p1"
  "-I"
  "coq/../coq-core/plugins/tutorial/p2"
  "-I"
  "coq/../coq-core/plugins/tutorial/p3"
  "-I"
  "coq/../coq-core/plugins/zify"
  "-R"
  "coq/theories"
  "Coq"
  "-R"
  "."
  "foo"
  "foo.v"


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

  $ dune build --trace-file trace.json foo.vo
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  $ runFlags
  "--config"
  "-type-in-type"
  "-w"
  "-deprecated-native-compiler-option"
  "-w"
  "-native-compiler-disabled"
  "-native-compiler"
  "ondemand"
  "-I"
  "coq/../coq-core/plugins/btauto"
  "-I"
  "coq/../coq-core/plugins/cc"
  "-I"
  "coq/../coq-core/plugins/derive"
  "-I"
  "coq/../coq-core/plugins/extraction"
  "-I"
  "coq/../coq-core/plugins/firstorder"
  "-I"
  "coq/../coq-core/plugins/funind"
  "-I"
  "coq/../coq-core/plugins/ltac"
  "-I"
  "coq/../coq-core/plugins/ltac2"
  "-I"
  "coq/../coq-core/plugins/micromega"
  "-I"
  "coq/../coq-core/plugins/nsatz"
  "-I"
  "coq/../coq-core/plugins/number_string_notation"
  "-I"
  "coq/../coq-core/plugins/ring"
  "-I"
  "coq/../coq-core/plugins/rtauto"
  "-I"
  "coq/../coq-core/plugins/ssreflect"
  "-I"
  "coq/../coq-core/plugins/ssrmatching"
  "-I"
  "coq/../coq-core/plugins/tauto"
  "-I"
  "coq/../coq-core/plugins/tutorial/p0"
  "-I"
  "coq/../coq-core/plugins/tutorial/p1"
  "-I"
  "coq/../coq-core/plugins/tutorial/p2"
  "-I"
  "coq/../coq-core/plugins/tutorial/p3"
  "-I"
  "coq/../coq-core/plugins/zify"
  "-R"
  "coq/theories"
  "Coq"
  "-R"
  "."
  "foo"
  "foo.v"

TC: extend in workspace + override standard

  $ cat > dune-workspace <<EOF
  > (lang dune 2.8)
  > (env (dev (coq (flags :standard -type-in-type))))
  > EOF

  $ dune build --trace-file trace.json foo.vo
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  $ runFlags
  "--config"
  "-q"
  "-type-in-type"
  "-w"
  "-deprecated-native-compiler-option"
  "-w"
  "-native-compiler-disabled"
  "-native-compiler"
  "ondemand"
  "-I"
  "coq/../coq-core/plugins/btauto"
  "-I"
  "coq/../coq-core/plugins/cc"
  "-I"
  "coq/../coq-core/plugins/derive"
  "-I"
  "coq/../coq-core/plugins/extraction"
  "-I"
  "coq/../coq-core/plugins/firstorder"
  "-I"
  "coq/../coq-core/plugins/funind"
  "-I"
  "coq/../coq-core/plugins/ltac"
  "-I"
  "coq/../coq-core/plugins/ltac2"
  "-I"
  "coq/../coq-core/plugins/micromega"
  "-I"
  "coq/../coq-core/plugins/nsatz"
  "-I"
  "coq/../coq-core/plugins/number_string_notation"
  "-I"
  "coq/../coq-core/plugins/ring"
  "-I"
  "coq/../coq-core/plugins/rtauto"
  "-I"
  "coq/../coq-core/plugins/ssreflect"
  "-I"
  "coq/../coq-core/plugins/ssrmatching"
  "-I"
  "coq/../coq-core/plugins/tauto"
  "-I"
  "coq/../coq-core/plugins/tutorial/p0"
  "-I"
  "coq/../coq-core/plugins/tutorial/p1"
  "-I"
  "coq/../coq-core/plugins/tutorial/p2"
  "-I"
  "coq/../coq-core/plugins/tutorial/p3"
  "-I"
  "coq/../coq-core/plugins/zify"
  "-R"
  "coq/theories"
  "Coq"
  "-R"
  "."
  "foo"
  "foo.v"

TC: extend in dune (env) + override standard

  $ cat > dune <<EOF
  > (coq.theory
  >  (name foo))
  > (env (dev (coq (flags -type-in-type))))
  > EOF

  $ rm -rf _build/default/foo.vo
  $ dune build --trace-file trace.json foo.vo
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  $ runFlags
  "--config"
  "-type-in-type"
  "-w"
  "-deprecated-native-compiler-option"
  "-w"
  "-native-compiler-disabled"
  "-native-compiler"
  "ondemand"
  "-I"
  "coq/../coq-core/plugins/btauto"
  "-I"
  "coq/../coq-core/plugins/cc"
  "-I"
  "coq/../coq-core/plugins/derive"
  "-I"
  "coq/../coq-core/plugins/extraction"
  "-I"
  "coq/../coq-core/plugins/firstorder"
  "-I"
  "coq/../coq-core/plugins/funind"
  "-I"
  "coq/../coq-core/plugins/ltac"
  "-I"
  "coq/../coq-core/plugins/ltac2"
  "-I"
  "coq/../coq-core/plugins/micromega"
  "-I"
  "coq/../coq-core/plugins/nsatz"
  "-I"
  "coq/../coq-core/plugins/number_string_notation"
  "-I"
  "coq/../coq-core/plugins/ring"
  "-I"
  "coq/../coq-core/plugins/rtauto"
  "-I"
  "coq/../coq-core/plugins/ssreflect"
  "-I"
  "coq/../coq-core/plugins/ssrmatching"
  "-I"
  "coq/../coq-core/plugins/tauto"
  "-I"
  "coq/../coq-core/plugins/tutorial/p0"
  "-I"
  "coq/../coq-core/plugins/tutorial/p1"
  "-I"
  "coq/../coq-core/plugins/tutorial/p2"
  "-I"
  "coq/../coq-core/plugins/tutorial/p3"
  "-I"
  "coq/../coq-core/plugins/zify"
  "-R"
  "coq/theories"
  "Coq"
  "-R"
  "."
  "foo"
  "foo.v"

TC: extend in dune (env) + standard

  $ cat > dune <<EOF
  > (coq.theory
  >  (name foo))
  > (env (dev (coq (flags :standard -type-in-type))))
  > EOF

  $ rm -rf _build/default/foo.vo
  $ dune build --trace-file trace.json foo.vo
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  $ runFlags
  "--config"
  "-q"
  "-type-in-type"
  "-type-in-type"
  "-w"
  "-deprecated-native-compiler-option"
  "-w"
  "-native-compiler-disabled"
  "-native-compiler"
  "ondemand"
  "-I"
  "coq/../coq-core/plugins/btauto"
  "-I"
  "coq/../coq-core/plugins/cc"
  "-I"
  "coq/../coq-core/plugins/derive"
  "-I"
  "coq/../coq-core/plugins/extraction"
  "-I"
  "coq/../coq-core/plugins/firstorder"
  "-I"
  "coq/../coq-core/plugins/funind"
  "-I"
  "coq/../coq-core/plugins/ltac"
  "-I"
  "coq/../coq-core/plugins/ltac2"
  "-I"
  "coq/../coq-core/plugins/micromega"
  "-I"
  "coq/../coq-core/plugins/nsatz"
  "-I"
  "coq/../coq-core/plugins/number_string_notation"
  "-I"
  "coq/../coq-core/plugins/ring"
  "-I"
  "coq/../coq-core/plugins/rtauto"
  "-I"
  "coq/../coq-core/plugins/ssreflect"
  "-I"
  "coq/../coq-core/plugins/ssrmatching"
  "-I"
  "coq/../coq-core/plugins/tauto"
  "-I"
  "coq/../coq-core/plugins/tutorial/p0"
  "-I"
  "coq/../coq-core/plugins/tutorial/p1"
  "-I"
  "coq/../coq-core/plugins/tutorial/p2"
  "-I"
  "coq/../coq-core/plugins/tutorial/p3"
  "-I"
  "coq/../coq-core/plugins/zify"
  "-R"
  "coq/theories"
  "Coq"
  "-R"
  "."
  "foo"
  "foo.v"

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
  $ dune build --trace-file trace.json foo.vo
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  $ runFlags
  "--config"
  "-q"
  "-type-in-type"
  "-bt"
  "-w"
  "-deprecated-native-compiler-option"
  "-w"
  "-native-compiler-disabled"
  "-native-compiler"
  "ondemand"
  "-I"
  "coq/../coq-core/plugins/btauto"
  "-I"
  "coq/../coq-core/plugins/cc"
  "-I"
  "coq/../coq-core/plugins/derive"
  "-I"
  "coq/../coq-core/plugins/extraction"
  "-I"
  "coq/../coq-core/plugins/firstorder"
  "-I"
  "coq/../coq-core/plugins/funind"
  "-I"
  "coq/../coq-core/plugins/ltac"
  "-I"
  "coq/../coq-core/plugins/ltac2"
  "-I"
  "coq/../coq-core/plugins/micromega"
  "-I"
  "coq/../coq-core/plugins/nsatz"
  "-I"
  "coq/../coq-core/plugins/number_string_notation"
  "-I"
  "coq/../coq-core/plugins/ring"
  "-I"
  "coq/../coq-core/plugins/rtauto"
  "-I"
  "coq/../coq-core/plugins/ssreflect"
  "-I"
  "coq/../coq-core/plugins/ssrmatching"
  "-I"
  "coq/../coq-core/plugins/tauto"
  "-I"
  "coq/../coq-core/plugins/tutorial/p0"
  "-I"
  "coq/../coq-core/plugins/tutorial/p1"
  "-I"
  "coq/../coq-core/plugins/tutorial/p2"
  "-I"
  "coq/../coq-core/plugins/tutorial/p3"
  "-I"
  "coq/../coq-core/plugins/zify"
  "-R"
  "coq/theories"
  "Coq"
  "-R"
  "."
  "foo"
  "foo.v"
