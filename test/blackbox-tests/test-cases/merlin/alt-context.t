Showcase behavior when passing the `--context` flag to ocaml-merlin

  $ make_dune_project 3.14

  $ make_two_context_workspace 3.14 alt

  $ lib1=foo
  $ lib2=bar
  $ cat >dune <<EOF
  > (library
  >  (name $lib1)
  >  (modules $lib1)
  >  (enabled_if (= %{context_name} "default")))
  > (library
  >  (name $lib2)
  >  (modules $lib2)
  >  (enabled_if (= %{context_name} "alt")))
  > EOF

  $ touch $lib1.ml $lib2.ml

  $ dune build

  $ FILE1=$PWD/$lib1.ml
  $ FILE2=$PWD/$lib2.ml

If `generate_merlin_rules` is not used, we can't query anything in alt context
because by default Merlin rules are only created for the default context

  $ query_ocaml_merlin_pp "$FILE2" | grep -i "$lib2"
  ((ERROR "No config found for file bar.ml. Try calling 'dune build'."))

  $ query_ocaml_merlin_pp "$FILE2" --context alt | grep -i "$lib2"
  ((ERROR "No config found for file bar.ml. Try calling 'dune build'."))

Let's use `generate_merlin_rules` to test these commands

  $ cat > dune-workspace << EOF
  > (lang dune 3.16)
  > 
  > (context default)
  > 
  > (context
  >  (default
  >   (name alt)
  >   (generate_merlin_rules)))
  > EOF

  $ dune build

Request config for file in alt context without using --context

  $ query_ocaml_merlin_pp "$FILE2" | grep -i "$lib2" | sed 's/^[^:]*:[^:]*://'
  ((ERROR "No config found for file bar.ml. Try calling 'dune build'."))

Request config for file in alt context using --context

  $ query_ocaml_merlin_pp "$FILE2" --context alt | grep -i "$lib2" | sed 's/^[^:]*:[^:]*://'
  ((INDEX $TESTCASE_ROOT/_build/alt/.bar.objs/cctx.ocaml-index)
   (B $TESTCASE_ROOT/_build/alt/.bar.objs/byte)
   (UNIT_NAME bar))

Request config for default context without using --context

  $ query_ocaml_merlin_pp "$FILE1" | grep -i "$lib1" | sed 's/^[^:]*:[^:]*://'
   (INDEX $TESTCASE_ROOT/_build/default/.foo.objs/cctx.ocaml-index)
   (B $TESTCASE_ROOT/_build/default/.foo.objs/byte)
   (UNIT_NAME foo))

Request config for default context using --context

  $ query_ocaml_merlin_pp "$FILE1" --context alt | grep -i "$lib1" | sed 's/^[^:]*:[^:]*://'
  ((ERROR "No config found for file foo.ml. Try calling 'dune build'."))
