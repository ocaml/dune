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
