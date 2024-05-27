  $ ocamlc_where="$(ocamlc -where)"
  $ export BUILD_PATH_PREFIX_MAP="/OCAMLC_WHERE=$ocamlc_where:$BUILD_PATH_PREFIX_MAP"

A utility to query merlin configuration for a file:
  $ cat >merlin_conf.sh <<EOF
  > #!/bin/sh
  > FILE=\$1
  > printf "(4:File%d:%s)" \${#FILE} \$FILE | dune ocaml-merlin \
  >   | sed -E "s/[[:digit:]]+:/\?:/g" | sed -E "s/\)(\(+)/\n\1/g"
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
  ((?:INDEX?:$TESTCASE_ROOT/_build/default/.test.eobjs/cctx.ocaml-index
  (?:STDLIB?:/OCAMLC_WHERE
  (?:SOURCE_ROOT?:$TESTCASE_ROOT
  (?:EXCLUDE_QUERY_DIR
  (?:B?:$TESTCASE_ROOT/_build/default/.test.eobjs/byte
  (?:S?:$TESTCASE_ROOT
  (?:FLG(?:-open?:Dune__exe)
  (?:FLG(?:-pp?:$TESTCASE_ROOT/_build/default/pp.sh)
  (?:FLG(?:-w?:@1..3@5..28@31..39@43@46..47@49..57@61..62@67@69-?:-strict-sequence?:-strict-formats?:-short-paths?:-keep-locs?:-no-strict-formats?:-g)
  (?:UNIT_NAME?:dune__exe__Pped
  (?:SUFFIX?:.mlx .mlx))

  $ ./merlin_conf.sh pped.mli | diff pped.out -

Melange:

As expected, the reader is not communicated for the standard mli
  $ ./merlin_conf.sh mel.mli | tee mel.out
  ((?:INDEX?:$TESTCASE_ROOT/_build/default/.test.eobjs/cctx.ocaml-index
  (?:STDLIB?:/OCAMLC_WHERE
  (?:SOURCE_ROOT?:$TESTCASE_ROOT
  (?:EXCLUDE_QUERY_DIR
  (?:B?:$TESTCASE_ROOT/_build/default/.test.eobjs/byte
  (?:S?:$TESTCASE_ROOT
  (?:FLG(?:-open?:Dune__exe)
  (?:FLG(?:-w?:@1..3@5..28@31..39@43@46..47@49..57@61..62@67@69-?:-strict-sequence?:-strict-formats?:-short-paths?:-keep-locs?:-no-strict-formats?:-g)
  (?:UNIT_NAME?:dune__exe__Mel
  (?:SUFFIX?:.mlx .mlx))

The reader is set for the mlx file
  $ ./merlin_conf.sh mel.mlx | diff mel.out -
  10c10,11
  < (?:SUFFIX?:.mlx .mlx))
  \ No newline at end of file
  ---
  > (?:SUFFIX?:.mlx .mlx
  > (?:READER(?:mlx)))
  \ No newline at end of file
  [1]

Unconventional file names:

Users might have preprocessing steps that start with a non-conventional
filename like `mymodule.cppo.ml`. 

While Dune first tries to match by the exact filename requested, if nothing is
found, then it'll make a guess that the file was preprocessed into a file with
.ml extension:

  $ ./merlin_conf.sh cppomod.cppo.ml | tee cppomod.out
  ((?:INDEX?:$TESTCASE_ROOT/_build/default/.test.eobjs/cctx.ocaml-index
  (?:STDLIB?:/OCAMLC_WHERE
  (?:SOURCE_ROOT?:$TESTCASE_ROOT
  (?:EXCLUDE_QUERY_DIR
  (?:B?:$TESTCASE_ROOT/_build/default/.test.eobjs/byte
  (?:S?:$TESTCASE_ROOT
  (?:FLG(?:-open?:Dune__exe)
  (?:FLG(?:-w?:@1..3@5..28@31..39@43@46..47@49..57@61..62@67@69-?:-strict-sequence?:-strict-formats?:-short-paths?:-keep-locs?:-no-strict-formats?:-g)
  (?:UNIT_NAME?:dune__exe__Cppomod
  (?:SUFFIX?:.mlx .mlx))

  $ ./merlin_conf.sh cppomod.ml | diff cppomod.out -

Note that this means unrelated files might be given the same configuration:

  $ ./merlin_conf.sh cppomod.tralala.ml | diff cppomod.out -

And with unconventional extension: 
(note that without appropriate suffix configuration Merlin will never jump to
such files) 
We could expect dune to get the wrongext module configuration
  $ ./merlin_conf.sh wrongext.cppo.cml | tee wrongext.out
  ((?:INDEX?:$TESTCASE_ROOT/_build/default/.test.eobjs/cctx.ocaml-index
  (?:STDLIB?:/OCAMLC_WHERE
  (?:SOURCE_ROOT?:$TESTCASE_ROOT
  (?:EXCLUDE_QUERY_DIR
  (?:B?:$TESTCASE_ROOT/_build/default/.test.eobjs/byte
  (?:S?:$TESTCASE_ROOT
  (?:FLG(?:-open?:Dune__exe)
  (?:FLG(?:-w?:@1..3@5..28@31..39@43@46..47@49..57@61..62@67@69-?:-strict-sequence?:-strict-formats?:-short-paths?:-keep-locs?:-no-strict-formats?:-g)
  (?:UNIT_NAME?:dune__exe__Wrongext
  (?:SUFFIX?:.mlx .mlx))

We also have generated.ml and generatedx.mlx promoted:
  $ ls -1 . | grep generated
  generated.ml
  generatedx.mlx

It should be possible to get its merlin configuration as well:
  $ ./merlin_conf.sh generated.ml
  ((?:INDEX?:$TESTCASE_ROOT/_build/default/.test.eobjs/cctx.ocaml-index
  (?:STDLIB?:/OCAMLC_WHERE
  (?:SOURCE_ROOT?:$TESTCASE_ROOT
  (?:EXCLUDE_QUERY_DIR
  (?:B?:$TESTCASE_ROOT/_build/default/.test.eobjs/byte
  (?:S?:$TESTCASE_ROOT
  (?:FLG(?:-open?:Dune__exe)
  (?:FLG(?:-w?:@1..3@5..28@31..39@43@46..47@49..57@61..62@67@69-?:-strict-sequence?:-strict-formats?:-short-paths?:-keep-locs?:-no-strict-formats?:-g)
  (?:UNIT_NAME?:dune__exe__Generated
  (?:SUFFIX?:.mlx .mlx))
  $ ./merlin_conf.sh generatedx.mlx
  ((?:INDEX?:$TESTCASE_ROOT/_build/default/.test.eobjs/cctx.ocaml-index
  (?:STDLIB?:/OCAMLC_WHERE
  (?:SOURCE_ROOT?:$TESTCASE_ROOT
  (?:EXCLUDE_QUERY_DIR
  (?:B?:$TESTCASE_ROOT/_build/default/.test.eobjs/byte
  (?:S?:$TESTCASE_ROOT
  (?:FLG(?:-open?:Dune__exe)
  (?:FLG(?:-w?:@1..3@5..28@31..39@43@46..47@49..57@61..62@67@69-?:-strict-sequence?:-strict-formats?:-short-paths?:-keep-locs?:-no-strict-formats?:-g)
  (?:UNIT_NAME?:dune__exe__Generatedx
  (?:SUFFIX?:.mlx .mlx
  (?:READER(?:mlx)))
