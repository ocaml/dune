Testing coqdoc when composed with a boot library

  $ dune build A/A.html
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))

  $ ls _build/default/A
  A.html
  a.glob
  a.v
  a.vo
  a.vok
  a.vos

Dune should be passing '--coqlib' to coqdoc, but it doesn't. This is a bug.

  $ cat _build/log | sed 's/$ (cd .*coqc/coqc/' | sed 's/$ (cd .*coqdoc/coqdoc/' | sed '/# /d' | sed '/> /d' | sort
  coqc -q -w -deprecated-native-compiler-option -w -native-compiler-disabled -native-compiler ondemand -boot -R Coq Coq Coq/mytheory.v)
  coqc -q -w -deprecated-native-compiler-option -w -native-compiler-disabled -native-compiler ondemand -noinit -boot -R Coq Coq -R A A A/a.v)
  coqc -q -w -deprecated-native-compiler-option -w -native-compiler-disabled -native-compiler ondemand -noinit -boot -R Coq Coq Coq/Init/Prelude.v)
  coqdoc -R ../Coq Coq -R . A --toc --html -d A.html a.v)
