Testing coqdoc when composed with a boot library

  $ dune build A/A.html

  $ ls _build/default/A
  A.html
  a.glob
  a.v
  a.vo
  a.vok
  a.vos

Dune should be passing '--coqlib' to coqdoc, but it doesn't. This is a bug.

  $ cat _build/log | sed 's/$ (cd .*rocq/rocq/' | sed '/# /d' | sed '/> /d' | sort
  rocq compile -q -w -deprecated-native-compiler-option -w -native-compiler-disabled -native-compiler ondemand -boot -R Coq Corelib Coq/mytheory.v)
  rocq compile -q -w -deprecated-native-compiler-option -w -native-compiler-disabled -native-compiler ondemand -noinit -boot -R Coq Corelib -R A A A/a.v)
  rocq compile -q -w -deprecated-native-compiler-option -w -native-compiler-disabled -native-compiler ondemand -noinit -boot -R Coq Corelib Coq/Init/Prelude.v)
  rocq doc -R ../Coq Corelib -R . A --toc --html -d A.html a.v)
