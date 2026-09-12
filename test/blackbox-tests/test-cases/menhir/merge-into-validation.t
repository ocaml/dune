Invalid merge_into names currently cause internal errors during rule generation.

  $ make_menhir_project 3.21 3.0

  $ cat >dune <<EOF
  > (menhir
  >  (merge_into ../parser)
  >  (modules parser))
  > EOF

  $ dune build 2>&1 | sed '/^Raised at/,$d'
  Internal error! Please report to https://github.com/ocaml/dune/issues,
  providing the file _build/trace.csexp, if possible. This includes build
  commands, message logs, and file paths.
  Description:
    ("[gen_rules] returned rules in a directory that is not a descendant of the directory it was called for",
     { dir = In_build_dir "default"
     ; example =
         Rule
           { targets =
               { root = In_build_dir "."
               ; files = set { "parser.ml"; "parser.mli" }
               ; dirs = set {}
               }
           }
     })
  [1]

  $ cat >dune <<EOF
  > (menhir
  >  (merge_into "")
  >  (modules parser))
  > EOF

  $ dune build 2>&1 | sed '/^Raised at/,$d'
  Internal error! Please report to https://github.com/ocaml/dune/issues,
  providing the file _build/trace.csexp, if possible. This includes build
  commands, message logs, and file paths.
  Description:
    ("[gen_rules] returned rules in a directory that is not a descendant of the directory it was called for",
     { dir = In_build_dir "default"
     ; example =
         Rule
           { targets =
               { root = In_build_dir "."
               ; files = set { "default.ml"; "default.mli" }
               ; dirs = set {}
               }
           }
     })
  [1]
