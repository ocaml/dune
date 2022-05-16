
  $ dune exec ./main.exe
  156

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

  $ ocaml-uideps dump ./_build/default/project.uideps
  {uid: Dune__exe__Othermod.0; locs:
     "y": File "$TESTCASE_ROOT/othermod.ml", line 1, characters 4-5;
     "y": File "$TESTCASE_ROOT/othermod.ml", line 2, characters 17-18
   uid: Otherlib.1; locs:
     "do_something": File "$TESTCASE_ROOT/lib/otherlib.ml", line 2, characters 4-16
   uid: Dune__exe__Othermod.1; locs:
     "other": File "$TESTCASE_ROOT/main.ml", line 1, characters 20-25;
     "other": File "$TESTCASE_ROOT/othermod.ml", line 2, characters 4-9
   uid: Imp_lib.0; locs:
     "imp_x": File "$TESTCASE_ROOT/implicit-lib/imp_lib.ml", line 1, characters 4-9;
     "Otherlib.imp_x": File "$TESTCASE_ROOT/main.ml", line 1, characters 52-66
   uid: Otherlib.0; locs:
     "fromotherlib": File "$TESTCASE_ROOT/lib/otherlib.ml", line 1, characters 4-16;
     "fromotherlib": File "$TESTCASE_ROOT/lib/otherlib.ml", line 2, characters 29-41;
     "Otherlib.fromotherlib": File "$TESTCASE_ROOT/main.ml", line 1, characters 28-49
   uid: Stdlib.313; locs:
     "print_int": File "$TESTCASE_ROOT/main.ml", line 1, characters 0-9
   uid: Stdlib.55; locs:
     "+": File "$TESTCASE_ROOT/main.ml", line 1, characters 26-27;
     "+": File "$TESTCASE_ROOT/main.ml", line 1, characters 50-51;
     "+": File "$TESTCASE_ROOT/othermod.ml", line 2, characters 15-16
   uid: Stdlib.141; locs:
     "ignore": File "$TESTCASE_ROOT/lib/otherlib.ml", line 2, characters 22-28
   }
  And 0 partial shapes.
