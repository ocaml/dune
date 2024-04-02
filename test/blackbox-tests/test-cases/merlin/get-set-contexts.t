Showcase behavior of the GetContexts ocaml-merlin command

  $ cat >dune-project <<EOF
  > (lang dune 3.14)
  > EOF

  $ cat > dune-workspace << EOF
  > (lang dune 3.14)
  > 
  > (context default)
  > 
  > (context
  >  (default
  >   (name alt)))
  > EOF

  $ printf "(11:GetContexts)" | dune ocaml-merlin
  (3:alt7:default)

Showcase behavior of the SetContext ocaml-merlin command

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

  $ printf '(4:File%d:%s)' ${#FILE2} $FILE2 | dune ocaml-merlin | dune format-dune-file | grep -i "$lib2"
  ((5:ERROR58:No config found for file bar.ml. Try calling 'dune build'.))

  $ printf '(10:SetContext3:alt)(4:File%d:%s)' ${#FILE2} $FILE2 | dune ocaml-merlin | dune format-dune-file | grep -i "$lib2"
  ((5:ERROR58:No config found for file bar.ml. Try calling 'dune build'.))

Let's use `generate_merlin_rules` to test these commands

  $ cat > dune-workspace << EOF
  > (lang dune 3.14)
  > 
  > (context default)
  > 
  > (context
  >  (default
  >   (name alt)
  >   (generate_merlin_rules)))
  > EOF

  $ dune build

Request config for file in alt context before calling SetContext

  $ printf '(4:File%d:%s)' ${#FILE2} $FILE2 | dune ocaml-merlin | dune format-dune-file | grep -i "$lib2"
  ((5:ERROR58:No config found for file bar.ml. Try calling 'dune build'.))

Request config for file in alt context after calling SetContext

  $ printf '(10:SetContext3:alt)(4:File%d:%s)' ${#FILE2} $FILE2 | dune ocaml-merlin | dune format-dune-file | grep -i "$lib2"
   (1:B139:$TESTCASE_ROOT/_build/alt/.bar.objs/byte)

Both together

  $ printf '(4:File%d:%s)(10:SetContext3:alt)(4:File%d:%s)' ${#FILE2} $FILE2 ${#FILE2} $FILE2 | dune ocaml-merlin | dune format-dune-file | grep -i "$lib2"
  ((5:ERROR58:No config found for file bar.ml. Try calling 'dune build'.))
   (1:B139:$TESTCASE_ROOT/_build/alt/.bar.objs/byte)

Request config for default context before and after calling SetContext

  $ printf '(4:File%d:%s)(10:SetContext3:alt)(4:File%d:%s)' ${#FILE1} $FILE1 ${#FILE1} $FILE1 | dune ocaml-merlin | dune format-dune-file | grep -i "$lib1"
   (1:B143:$TESTCASE_ROOT/_build/default/.foo.objs/byte)
  ((5:ERROR58:No config found for file foo.ml. Try calling 'dune build'.))
