Compilation using jsoo

  $ dune build rewriter1/pp5+dump.byte rewriter1/pp5+o.byte
      mkcamlp5 rewriter1/pp5+dump.byte
  ocamlfind: [WARNING] Package `camlp5.extend': camlp5.extend SHOULD NOT be used with camlp5.pa_o
      mkcamlp5 rewriter1/pp5+o.byte
  ocamlfind: [WARNING] Package `camlp5.extend': camlp5.extend SHOULD NOT be used with camlp5.pa_o
$ not-ocamlfind preprocess -syntax camlp5o -package camlp5.pr_o,camlp5.extend,camlp5.quotations rewriter1/extension.ml
$ dune exec rewriter1/pp5+o.byte rewriter1/extension.ml
Magic below to disable machine-dependent paths
See https://stackoverflow.com/a/3618308/1065436
$ dune build  rewriter1/rewriter1.exe 3>&1 1>&2 2>&3 3>&-  | sed '/Interface topdirs.cmi occurs in several directories/d'
$ dune build bin/test1.exe
$ dune exec bin/test1.exe
  $ dune build  rewriter3ppx/librewriter3.cma --verbose
$ dune exec rewriter1/rewriter1.exe bin/test1.ml
$ dune exec rewriter1/rewriter1.exe bin/test1.ml | ocaml -stdin
  $ dune build rewriter2/librewriter2.cmxa --verbose #--debug-backtraces


Attempt 2
$ not-ocamlfind preprocess -syntax camlp5 -package camlp5.pa_r,camlp5.pr_r rewriter2/lib2.ml
