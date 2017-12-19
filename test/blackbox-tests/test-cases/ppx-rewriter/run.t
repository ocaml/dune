  $ $JBUILDER build ./w_omp_driver.exe -j1 --root .
      ocamldep ppx/fooppx.depends.ocamldep-output
        ocamlc ppx/fooppx.{cmi,cmo,cmt}
      ocamlopt ppx/fooppx.{cmx,o}
      ocamlopt ppx/fooppx.{a,cmxa}
      ocamlopt .ppx/fooppx/ppx.exe
           ppx w_omp_driver.pp.ml
      ocamldep w_omp_driver.depends.ocamldep-output
        ocamlc w_omp_driver.{cmi,cmo,cmt}
      ocamlopt w_omp_driver.{cmx,o}
      ocamlopt w_omp_driver.exe
  $ $JBUILDER build ./w_ppx_driver.exe -j1 --root .
      ocamlopt .ppx/ppx_driver.runner/ppx.exe
           ppx w_ppx_driver.pp.ml
      ocamldep w_ppx_driver.depends.ocamldep-output
        ocamlc w_ppx_driver.{cmi,cmo,cmt}
      ocamlopt w_ppx_driver.{cmx,o}
      ocamlopt w_ppx_driver.exe
  $ $JBUILDER build ./w_ppx_driver_flags.exe -j1 --root .
      ocamlopt .ppx/fooppx+ppx_driver.runner/ppx.exe
           ppx w_ppx_driver_flags.pp.ml (exit 2)
  (cd _build/default && ./.ppx/fooppx+ppx_driver.runner/ppx.exe -flag -arg omp --dump-ast -o w_ppx_driver_flags.pp.ml --impl w_ppx_driver_flags.ml)
  ./.ppx/fooppx+ppx_driver.runner/ppx.exe: unknown option '-flag'.
  ppx.exe [extra_args] [<files>]
    -loc-filename <string>      File name to use in locations
    -reserve-namespace <string> Mark the given namespace as reserved
    -no-check                   Disable checks (unsafe)
    -do-check                   Enable checks
    -apply <names>              Apply these transformations in order (comma-separated list)
    -dont-apply <names>         Exclude these transformations
    -no-merge                   Do not merge context free transformations (better for debugging rewriters)
    -as-ppx                     Run as a -ppx rewriter (must be the first argument)
    --as-ppx                    Same as -as-ppx
    -as-pp                      Shorthand for: -dump-ast -embed-errors
    --as-pp                     Same as -as-pp
    -o <filename>               Output file (use '-' for stdout)
    -                           Read input from stdin
    -no-optcomp                 Do not use optcomp (default if the input or output of -pp is a binary AST)
    -dump-ast                   Dump the marshaled ast to the output file instead of pretty-printing it
    --dump-ast                  Same as -dump-ast
    -dparsetree                 Print the parsetree (same as ocamlc -dparsetree)
    -embed-errors               Embed errors in the output AST (default: true when -dump-ast, false otherwise)
    -null                       Produce no output, except for errors
    -impl <file>                Treat the input as a .ml file
    --impl <file>               Same as -impl
    -intf <file>                Treat the input as a .mli file
    --intf <file>               Same as -intf
    -debug-attribute-drop       Debug attribute dropping
    -print-transformations      Print linked-in code transformations, in the order they are applied
    -print-passes               Print the actual passes over the whole AST in the order they are applied
    -ite-check                  Enforce that "complex" if branches are delimited (disabled if -pp is given)
    -pp <command>               Pipe sources through preprocessor <command> (incompatible with -as-ppx)
    -reconcile                  (WIP) Pretty print the output using a mix of the input source and the generated code
    -reconcile-with-comments    (WIP) same as -reconcile but uses comments to enclose the generated code
    -no-color                   Don't use colors when printing errors
    -diff-cmd                   Diff command when using code expectations
    -pretty                     Instruct code generators to improve the prettiness of the generated code
    -styler                     Code styler
    -cookie NAME=EXPR           Set the cookie NAME to EXPR
    --cookie                    Same as -cookie
    -help                       Display this list of options
    --help                      Display this list of options
  [1]
