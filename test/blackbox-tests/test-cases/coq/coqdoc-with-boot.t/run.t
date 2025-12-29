Testing coqdoc when composed with a boot library

  $ dune build A/A.html
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))

  $ ls _build/default/A
  A.html
  a.glob
  a.v
  a.vo
  a.vok
  a.vos

Dune should be passing '--coqlib' to coqdoc, but it doesn't. This is a bug.

  $ dune trace cat | jq -c 'select(.name == "coqc" or .name == "coqdoc") | .args.process_args'
  ["--config"]
  ["-q","-w","-deprecated-native-compiler-option","-w","-native-compiler-disabled","-native-compiler","ondemand","-noinit","-boot","-R","Coq","Coq","Coq/Init/Prelude.v"]
  ["-q","-w","-deprecated-native-compiler-option","-w","-native-compiler-disabled","-native-compiler","ondemand","-boot","-R","Coq","Coq","Coq/mytheory.v"]
  ["-q","-w","-deprecated-native-compiler-option","-w","-native-compiler-disabled","-native-compiler","ondemand","-noinit","-boot","-R","Coq","Coq","-R","A","A","A/a.v"]
  ["-R","../Coq","Coq","-R",".","A","--toc","--html","-d","A.html","a.v"]
