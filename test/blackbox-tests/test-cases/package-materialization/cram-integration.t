Test that (deps (package ...)) in a cram stanza sets up all layout env vars.

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package (name mypkg))
  > EOF
  $ mkdir src
  $ cat >src/dune <<EOF
  > (library
  >  (public_name mypkg)
  >  (foreign_stubs (language c) (names stub)))
  > EOF
  $ cat >src/mypkg.ml <<'EOF'
  > let x = 1
  > EOF
  $ cat >src/stub.c <<'EOF'
  > #include <caml/mlvalues.h>
  > CAMLprim value mypkg_stub(value unit) { return Val_unit; }
  > EOF

Capture baselines into a setup script for the inner test:

  $ mkdir tests
  $ cat >tests/setup.sh <<EOF
  > env_added() {
  >   comm -23 <(tr ':' '\n' <<< "\$1" | sort -u) <(tr ':' '\n' <<< "\$2" | sort -u)
  > }
  > BASELINE_PATH='$PATH'
  > BASELINE_OCAMLPATH='$OCAMLPATH'
  > BASELINE_CAML_LD_LIBRARY_PATH='$CAML_LD_LIBRARY_PATH'
  > BASELINE_OCAMLFIND_IGNORE_DUPS_IN='$OCAMLFIND_IGNORE_DUPS_IN'
  > BASELINE_OCAMLTOP_INCLUDE_PATH='$OCAMLTOP_INCLUDE_PATH'
  > BASELINE_MANPATH='$MANPATH'
  > EOF
  $ cat >tests/dune <<EOF
  > (cram
  >  (shell bash)
  >  (deps (package mypkg))
  >  (setup_scripts setup.sh))
  > EOF
  $ cat >tests/check-env.t <<'EOF'
  >   $ env_added "$PATH" "$BASELINE_PATH"
  >   $ env_added "$OCAMLPATH" "$BASELINE_OCAMLPATH"
  >   $ env_added "$CAML_LD_LIBRARY_PATH" "$BASELINE_CAML_LD_LIBRARY_PATH"
  >   $ env_added "$OCAMLFIND_IGNORE_DUPS_IN" "$BASELINE_OCAMLFIND_IGNORE_DUPS_IN"
  >   $ env_added "$OCAMLTOP_INCLUDE_PATH" "$BASELINE_OCAMLTOP_INCLUDE_PATH"
  >   $ env_added "$MANPATH" "$BASELINE_MANPATH"
  > EOF

The inner test fails showing exactly what the package dep added to each var:

  $ dune runtest tests 2>&1
  File "tests/check-env.t", line 1, characters 0-0:
  --- tests/check-env.t
  +++ tests/check-env.t.corrected
  @@ -1,6 +1,12 @@
     $ env_added "$PATH" "$BASELINE_PATH"
  +  $TESTCASE_ROOT/_build/install/default/.packages/1dcd8c6ece3938b1f1fa14c3483ca989/bin
     $ env_added "$OCAMLPATH" "$BASELINE_OCAMLPATH"
  +  $TESTCASE_ROOT/_build/install/default/.packages/1dcd8c6ece3938b1f1fa14c3483ca989/lib
     $ env_added "$CAML_LD_LIBRARY_PATH" "$BASELINE_CAML_LD_LIBRARY_PATH"
  +  $TESTCASE_ROOT/_build/install/default/.packages/1dcd8c6ece3938b1f1fa14c3483ca989/lib/stublibs
     $ env_added "$OCAMLFIND_IGNORE_DUPS_IN" "$BASELINE_OCAMLFIND_IGNORE_DUPS_IN"
  +  $TESTCASE_ROOT/_build/install/default/.packages/1dcd8c6ece3938b1f1fa14c3483ca989/lib
     $ env_added "$OCAMLTOP_INCLUDE_PATH" "$BASELINE_OCAMLTOP_INCLUDE_PATH"
  +  $TESTCASE_ROOT/_build/install/default/.packages/1dcd8c6ece3938b1f1fa14c3483ca989/lib/toplevel
     $ env_added "$MANPATH" "$BASELINE_MANPATH"
  +  $TESTCASE_ROOT/_build/install/default/.packages/1dcd8c6ece3938b1f1fa14c3483ca989/man
  [1]
