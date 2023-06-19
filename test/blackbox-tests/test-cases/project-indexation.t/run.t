
  $ dune exec ./main.exe
  15642

  $ dune build @uideps

  $ find . -name '*.uideps'
  ./_build/default/implicit-lib/.imp_lib.objs/.uideps
  ./_build/default/implicit-lib/.imp_lib.objs/.uideps/imp_lib.cmt.uideps
  ./_build/default/implicit-lib/.imp_lib.objs/cctx.uideps
  ./_build/default/.main.eobjs/.uideps
  ./_build/default/.main.eobjs/.uideps/dune__exe__Main.cmt.uideps
  ./_build/default/.main.eobjs/.uideps/dune__exe.cmt.uideps
  ./_build/default/.main.eobjs/.uideps/dune__exe__Othermod.cmt.uideps
  ./_build/default/.main.eobjs/cctx.uideps
  ./_build/default/lib/.otherlib.objs/.uideps
  ./_build/default/lib/.otherlib.objs/.uideps/otherlib.cmt.uideps
  ./_build/default/lib/.otherlib.objs/cctx.uideps
  ./_build/default/project.uideps
  ./_build/default/vendor/otherproject/.vendored_lib.objs/.uideps
  ./_build/default/vendor/otherproject/.vendored_lib.objs/.uideps/vendored_lib.cmt.uideps
  ./_build/default/vendor/otherproject/.vendored_lib.objs/cctx.uideps

  $ ocaml-uideps dump ./_build/default/project.uideps
  14 uids:
  {uid: Dune__exe__Othermod.0; locs:
     "y": File "$TESTCASE_ROOT/othermod.ml", line 1, characters 4-5;
     "y": File "othermod.ml", line 2, characters 17-18
   uid: Dune__exe__Othermod; locs:
     "Dune__exe__Othermod": File ".main.eobjs/dune__exe.ml-gen", line 7, characters 18-37;
     "Othermod": File "main.ml", line 1, characters 10-18
   uid: Otherlib.1; locs:
     "do_something": File "$TESTCASE_ROOT/lib/otherlib.ml", line 2, characters 4-16
   uid: Dune__exe__Othermod.1; locs:
     "other": File "$TESTCASE_ROOT/othermod.ml", line 2, characters 4-9;
     "other": File "main.ml", line 1, characters 20-25
   uid: Stdlib.139; locs:
     "ignore": File "lib/otherlib.ml", line 2, characters 22-28
   uid: Imp_lib.0; locs:
     "imp_x": File "$TESTCASE_ROOT/implicit-lib/imp_lib.ml", line 1, characters 4-9;
     "Otherlib.imp_x": File "main.ml", line 1, characters 52-66
   uid: Imp_lib; locs:
     "Imp_lib": File "lib/otherlib.ml", line 4, characters 8-15
   uid: Otherlib.0; locs:
     "fromotherlib": File "$TESTCASE_ROOT/lib/otherlib.ml", line 1, characters 4-16;
     "fromotherlib": File "lib/otherlib.ml", line 2, characters 29-41;
     "Otherlib.fromotherlib": File "main.ml", line 1, characters 28-49
   uid: Stdlib.53; locs:
     "+": File "main.ml", line 1, characters 26-27;
     "+": File "main.ml", line 1, characters 50-51;
     "+": File "othermod.ml", line 2, characters 15-16
   uid: Vendored_lib.0; locs:
     "value": File "$TESTCASE_ROOT/vendor/otherproject/vendored_lib.ml", line 1, characters 4-9;
     "Vendored_lib.value": File "main.ml", line 2, characters 10-28
   uid: Dune__exe.0; locs:
     "Main": File "$TESTCASE_ROOT/.main.eobjs/dune__exe.ml-gen", line 4, characters 7-11
   uid: Dune__exe.1; locs:
     "Othermod": File "$TESTCASE_ROOT/.main.eobjs/dune__exe.ml-gen", line 7, characters 7-15
   uid: Stdlib.316; locs:
     "print_int": File "main.ml", line 1, characters 0-9;
     "print_int": File "main.ml", line 2, characters 0-9
   uid: Dune__exe__Main; locs:
     "Dune__exe__Main": File ".main.eobjs/dune__exe.ml-gen", line 4, characters 14-29
   }, 0 partial shapes: {}, 0 unreduced shapes: {} and shapes for CUS .
