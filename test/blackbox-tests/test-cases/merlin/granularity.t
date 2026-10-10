  $ ocamlc_where="$(ocamlc -where)"
  $ export BUILD_PATH_PREFIX_MAP="/OCAMLC_WHERE=$ocamlc_where:$BUILD_PATH_PREFIX_MAP"

A utility to query merlin configuration for a file:
  $ cat >merlin_conf.sh <<EOF
  > #!/bin/sh
  > FILE=\$1
  > query=\$(mktemp "\${TMPDIR:-.}/merlin-query.XXXXXX")
  > output=\$(mktemp "\${TMPDIR:-.}/merlin-output.XXXXXX")
  > printf '(File "%s")\n' "\$FILE" | dune internal sexp-to-csexp > "\$query"
  > dune ocaml-merlin < "\$query" > "\$output"
  > dune internal sexp-pp --format=csexp "\$output"
  > rm -f "\$query" "\$output"
  > EOF

  $ chmod a+x merlin_conf.sh

Project sources

  $ cat >dune-project <<EOF
  > (lang dune 3.16)
  > (using melange 0.1)
  > 
  > (dialect
  >  (name mlx)
  >  (implementation
  >   (extension mlx)
  >   (preprocess (run cat %{input-file}))
  >   (merlin_reader mlx)))
  > EOF

  $ cat >pp.sh <<EOF
  > #!/bin/sh
  > sed 's/%INT%/42/g' \$1
  > EOF

  $ chmod a+x pp.sh

  $ cat >dune <<EOF
  > (executable
  >  (name test)
  >  (flags :standard -no-strict-formats)
  >  (preprocess
  >   (per_module
  >    ((action (run ./pp.sh %{input-file})) pped))))
  > 
  > (rule
  >  (action  (copy cppomod.cppo.ml cppomod.ml)))
  > 
  > (rule
  >  (action  (copy wrongext.cppo.cml wrongext.ml)))
  > 
  > (rule
  >  (target generatedx.mlx)
  >  (mode promote)
  >  (action  (with-stdout-to %{target} (echo "let x = \"Generatedx!\""))))
  > 
  > (rule
  >  (target generated.ml)
  >  (mode promote)
  >  (action  (with-stdout-to %{target} (echo "let x = \"Generated!\""))))
  > EOF

  $ cat >test.ml <<EOF
  > print_endline Pped.x_42;;
  > print_endline Mel.x;;
  > print_endline Cppomod.x;;
  > print_endline Wrongext.x;;
  > print_endline Generated.x;;
  > EOF

Both Pped's ml and mli files will be preprocessed
  $ cat >pped.mli <<EOF
  > val x_%INT% : string
  > EOF

  $ cat >pped.ml <<EOF
  > let x_42 = "%INT%"
  > EOF

Melange module signature
  $ cat >mel.mli <<EOF
  > val x : string
  > EOF

Melange module implementation in Melange syntax
  $ cat >mel.mlx <<EOF
  > let x = "43"
  > EOF

A pped file with unconventionnal filename
  $ cat >cppomod.cppo.ml <<EOF
  > let x = "44"
  > EOF

  $ cat >wrongext.cppo.cml <<EOF
  > let x = "45"
  > EOF

  $ dune build @check
  $ dune exec ./test.exe
  42
  43
  44
  45
  Generated!

We now query Merlin configuration for the various source files:

Some configuration fields are common to all the modules of a same stanza. This
is the case for the stdlib, build and sources directories, flags and suffixes.

Some configuration can be specific to a module like preprocessing.

Dialects are specified by extensions so are specific to a file. This means a
different reader might be used for the signature and the implementation.

Note that Merlin should always be told about dialect-provided suffixes, to make `MerlinLocate` work correctly.

Preprocessing:

Is it expected that the suffix for implementation and interface is the same ?
  $ ./merlin_conf.sh pped.ml | tee pped.out
  ((INDEX $TESTCASE_ROOT/_build/default/.test.eobjs/cctx.ocaml-index)
   (STDLIB /OCAMLC_WHERE)
   (SOURCE_ROOT $TESTCASE_ROOT)
   (EXCLUDE_QUERY_DIR)
   (B $TESTCASE_ROOT/_build/default/.test.eobjs/byte)
   (S $TESTCASE_ROOT)
   (FLG
    (-w
     @1..3@5..28@31..39@43@46..47@49..57@61..62@67@69-40
     -strict-sequence
     -strict-formats
     -short-paths
     -keep-locs
     -no-strict-formats
     -g))
   (FLG
    (-pp $TESTCASE_ROOT/_build/default/pp.sh))
   (FLG
    (-open Dune__exe))
   (UNIT_NAME dune__exe__Pped)
   (SUFFIX ".mlx .mlx"))

  $ ./merlin_conf.sh pped.mli | diff pped.out -

Melange:

As expected, the reader is not communicated for the standard mli
  $ ./merlin_conf.sh mel.mli | tee mel.out
  ((INDEX $TESTCASE_ROOT/_build/default/.test.eobjs/cctx.ocaml-index)
   (STDLIB /OCAMLC_WHERE)
   (SOURCE_ROOT $TESTCASE_ROOT)
   (EXCLUDE_QUERY_DIR)
   (B $TESTCASE_ROOT/_build/default/.test.eobjs/byte)
   (S $TESTCASE_ROOT)
   (FLG
    (-w
     @1..3@5..28@31..39@43@46..47@49..57@61..62@67@69-40
     -strict-sequence
     -strict-formats
     -short-paths
     -keep-locs
     -no-strict-formats
     -g))
   (FLG
    (-open Dune__exe))
   (UNIT_NAME dune__exe__Mel)
   (SUFFIX ".mlx .mlx"))

The reader is set for the mlx file
  $ ./merlin_conf.sh mel.mlx | diff mel.out -
  19c19,20
  <  (SUFFIX ".mlx .mlx"))
  ---
  >  (SUFFIX ".mlx .mlx")
  >  (READER (mlx)))
  [1]

Unconventional file names:

Users might have preprocessing steps that start with a non-conventional
filename like `mymodule.cppo.ml`. 

While Dune first tries to match by the exact filename requested, if nothing is
found, then it'll make a guess that the file was preprocessed into a file with
.ml extension:

  $ ./merlin_conf.sh cppomod.cppo.ml | tee cppomod.out
  ((INDEX $TESTCASE_ROOT/_build/default/.test.eobjs/cctx.ocaml-index)
   (STDLIB /OCAMLC_WHERE)
   (SOURCE_ROOT $TESTCASE_ROOT)
   (EXCLUDE_QUERY_DIR)
   (B $TESTCASE_ROOT/_build/default/.test.eobjs/byte)
   (S $TESTCASE_ROOT)
   (FLG
    (-w
     @1..3@5..28@31..39@43@46..47@49..57@61..62@67@69-40
     -strict-sequence
     -strict-formats
     -short-paths
     -keep-locs
     -no-strict-formats
     -g))
   (FLG
    (-open Dune__exe))
   (UNIT_NAME dune__exe__Cppomod)
   (SUFFIX ".mlx .mlx"))

  $ ./merlin_conf.sh cppomod.ml | diff cppomod.out -

Note that this means unrelated files might be given the same configuration:

  $ ./merlin_conf.sh cppomod.tralala.ml | diff cppomod.out -

And with unconventional extension: 
(note that without appropriate suffix configuration Merlin will never jump to
such files) 
We could expect dune to get the wrongext module configuration
  $ ./merlin_conf.sh wrongext.cppo.cml | tee wrongext.out
  ((INDEX $TESTCASE_ROOT/_build/default/.test.eobjs/cctx.ocaml-index)
   (STDLIB /OCAMLC_WHERE)
   (SOURCE_ROOT $TESTCASE_ROOT)
   (EXCLUDE_QUERY_DIR)
   (B $TESTCASE_ROOT/_build/default/.test.eobjs/byte)
   (S $TESTCASE_ROOT)
   (FLG
    (-w
     @1..3@5..28@31..39@43@46..47@49..57@61..62@67@69-40
     -strict-sequence
     -strict-formats
     -short-paths
     -keep-locs
     -no-strict-formats
     -g))
   (FLG
    (-open Dune__exe))
   (UNIT_NAME dune__exe__Wrongext)
   (SUFFIX ".mlx .mlx"))

We also have generated.ml and generatedx.mlx promoted:
  $ ls -1 . | grep generated
  generated.ml
  generatedx.mlx

It should be possible to get its merlin configuration as well:
  $ ./merlin_conf.sh generated.ml
  ((INDEX $TESTCASE_ROOT/_build/default/.test.eobjs/cctx.ocaml-index)
   (STDLIB /OCAMLC_WHERE)
   (SOURCE_ROOT $TESTCASE_ROOT)
   (EXCLUDE_QUERY_DIR)
   (B $TESTCASE_ROOT/_build/default/.test.eobjs/byte)
   (S $TESTCASE_ROOT)
   (FLG
    (-w
     @1..3@5..28@31..39@43@46..47@49..57@61..62@67@69-40
     -strict-sequence
     -strict-formats
     -short-paths
     -keep-locs
     -no-strict-formats
     -g))
   (FLG
    (-open Dune__exe))
   (UNIT_NAME dune__exe__Generated)
   (SUFFIX ".mlx .mlx"))
  $ ./merlin_conf.sh generatedx.mlx
  ((INDEX $TESTCASE_ROOT/_build/default/.test.eobjs/cctx.ocaml-index)
   (STDLIB /OCAMLC_WHERE)
   (SOURCE_ROOT $TESTCASE_ROOT)
   (EXCLUDE_QUERY_DIR)
   (B $TESTCASE_ROOT/_build/default/.test.eobjs/byte)
   (S $TESTCASE_ROOT)
   (FLG
    (-w
     @1..3@5..28@31..39@43@46..47@49..57@61..62@67@69-40
     -strict-sequence
     -strict-formats
     -short-paths
     -keep-locs
     -no-strict-formats
     -g))
   (FLG
    (-open Dune__exe))
   (UNIT_NAME dune__exe__Generatedx)
   (SUFFIX ".mlx .mlx")
   (READER (mlx)))

An unconventional source keeps its configuration after adding an interface.

  $ printf 'val x : string\n' > wrongext.mli
  $ dune build @check
  $ ./merlin_conf.sh wrongext.ml > with-interface.out
  $ ./merlin_conf.sh wrongext.mli | diff with-interface.out -
  $ ./merlin_conf.sh wrongext.cppo.cml | diff with-interface.out -
  $ ./merlin_conf.sh wrongext | diff with-interface.out -

A copy-line directive can lead through a filename without an extension.

  $ mkdir copied
  $ cat > copied/dune-project <<EOF
  > (lang dune 3.16)
  > EOF
  $ cat > copied/dune <<EOF
  > (library
  >  (name copied)
  >  (modules actual))
  > (rule (action (copy# input.txt actual)))
  > (rule (action (copy actual actual.ml)))
  > EOF
  $ printf 'let value = 1\n' > copied/input.txt
  $ dune build --root copied @check
  $ query_ocaml_merlin_pp "$PWD/copied/actual.ml" --root copied > copied.out
  $ query_ocaml_merlin_pp "$PWD/copied/input.txt" --root copied | diff copied.out -

The typed lookup also follows the alias when its source kind is unambiguous.

  $ (cd copied && merlin_configurations _build/default/.merlin-conf/lib-copied input.txt)
  input.txt: ocaml true impl -

An ambiguous intermediate alias does not hide an exact copy destination from
the typed lookup.

  $ cat > copied/dune <<EOF
  > (library
  >  (name copied)
  >  (modules actual))
  > (rule (action (copy# input.txt actual)))
  > (rule (action (copy# actual actual.ml)))
  > EOF
  $ printf 'val value : int\n' > copied/actual.mli
  $ dune build --root copied @check
  $ (cd copied && merlin_configurations _build/default/.merlin-conf/lib-copied input.txt)
  input.txt: ocaml true impl actual.mli

The implicit executable interface is not an authored counterpart. Authored,
preprocessed sources retain their original counterpart paths.

  $ merlin_configurations _build/default/.merlin-conf/exe-test \
  >   test.ml pped.ml pped.mli
  test.ml: ocaml true impl -
  pped.ml: ocaml true impl pped.mli
  pped.mli: ocaml true intf pped.ml
  $ test -e test.mli
  [1]
  $ test -e _build/default/test.mli

An interface generated by a user rule is not an authored counterpart either.
Its authored implementation can be used as a counterpart.

  $ mkdir generated-interface
  $ cat > generated-interface/dune-project <<EOF
  > (lang dune 3.16)
  > EOF
  $ cat > generated-interface/dune <<EOF
  > (library (name generated))
  > (rule
  >  (mode fallback)
  >  (action (with-stdout-to generated.mli (echo ""))))
  > EOF
  $ touch generated-interface/generated.ml
  $ dune build --root generated-interface @check
  $ (cd generated-interface && merlin_configurations \
  >   _build/default/.merlin-conf/lib-generated generated.ml generated.mli)
  generated.ml: ocaml true impl -
  generated.mli: ocaml true intf generated.ml
  $ test -e generated-interface/generated.mli
  [1]
  $ test -e generated-interface/_build/default/generated.mli

Adding or removing an authored interface updates the counterpart metadata.

  $ touch generated-interface/generated.mli
  $ dune build --root generated-interface @check
  $ (cd generated-interface && merlin_configurations \
  >   _build/default/.merlin-conf/lib-generated generated.ml)
  generated.ml: ocaml true impl generated.mli
  $ rm generated-interface/generated.mli
  $ dune build --root generated-interface @check
  $ (cd generated-interface && merlin_configurations \
  >   _build/default/.merlin-conf/lib-generated generated.ml)
  generated.ml: ocaml true impl -

Implementation and interface filenames can differ in capitalization. Their
distinct fallback keys each identify a single source kind.

  $ mkdir distinct-aliases
  $ cat > distinct-aliases/dune-project <<EOF
  > (lang dune 3.16)
  > EOF
  $ cat > distinct-aliases/dune <<EOF
  > (library (name distinct) (modules value))
  > EOF
  $ printf 'let x = 1\n' > distinct-aliases/value.ml
  $ printf 'val x : int\n' > distinct-aliases/Value.mli
  $ dune build --root distinct-aliases @check
  $ (cd distinct-aliases && merlin_configurations \
  >   _build/default/.merlin-conf/lib-distinct \
  >   value.ml Value.mli value Value value.pp Value.pp)
  value.ml: ocaml true impl Value.mli
  Value.mli: ocaml true intf value.ml
  value: ocaml true impl Value.mli
  Value: ocaml true intf value.ml
  value.pp: ocaml true impl Value.mli
  Value.pp: ocaml true intf value.ml

Copy-line mappings to a unique fallback key work even if the module has both
source kinds. The legacy query still finds the same configuration.

  $ mv distinct-aliases/value.ml distinct-aliases/input.txt
  $ cat >> distinct-aliases/dune <<EOF
  > (rule (action (copy# input.txt value)))
  > (rule (action (copy value value.ml)))
  > EOF
  $ DUNE_SANDBOX=none dune build --root distinct-aliases @check
  $ (cd distinct-aliases && merlin_configurations \
  >   _build/default/.merlin-conf/lib-distinct input.txt value.ml Value.mli)
  input.txt: ocaml true impl Value.mli
  value.ml: ocaml true impl Value.mli
  Value.mli: ocaml true intf -
  $ query_ocaml_merlin_pp "$PWD/distinct-aliases/input.txt" --root distinct-aliases \
  >   | grep -Eo '\(UNIT_NAME [^)]*\)'
  (UNIT_NAME distinct__Value)
