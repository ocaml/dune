Test toplevel-init-file on a tiny project
----------------------------------------------------
  $ cat >dune-project <<EOF
  > (lang dune 2.1)
  > (name test)
  > EOF
  $ cat >dune <<EOF
  > (library
  >  (name test)
  >  (public_name test))
  > EOF
  $ touch test.opam
  $ cat >main.ml <<EOF
  > let hello () = print_endline "hello"
  > EOF

  $ dune ocaml top
  #directory "$TESTCASE_ROOT/_build/default/.test.objs/byte";;
  #load "$TESTCASE_ROOT/_build/default/test.cma";;

Absolute directory arguments are currently rejected.

  $ dune ocaml top "$PWD" 2>&1 | awk '/Internal error!/,/Raised at/'
  Internal error! Please report to https://github.com/ocaml/dune/issues,
  providing the file _build/trace.csexp, if possible. This includes build
  commands, message logs, and file paths.
  Description:
    ("Local.relative: received absolute path",
     { t = "default"
     ; path =
         "$TESTCASE_ROOT"
     })
  Raised at Stdune__Code_error.raise in file
  [1]

  $ ocaml -stdin <<EOF
  > #use_output "dune ocaml top";;
  > Test.Main.hello ();;
  > EOF
  hello

  $ cat >error.ml <<EOF
  > let oops () = undefined_function ()
  > EOF

  $ dune ocaml top
  File "error.ml", line 1, characters 14-32:
  1 | let oops () = undefined_function ()
                    ^^^^^^^^^^^^^^^^^^
  Error: Unbound value undefined_function
  [1]

  $ ocaml -stdin <<EOF
  > #use_output "dune ocaml top";;
  > EOF
  File "error.ml", line 1, characters 14-32:
  1 | let oops () = undefined_function ()
                    ^^^^^^^^^^^^^^^^^^
  Error: Unbound value undefined_function
  Command exited with code 1.
  [125]
