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

Request config for file in alt context before calling SetContext

  $ printf '(4:File%d:%s)' ${#FILE2} $FILE2 | dune ocaml-merlin | dune format-dune-file
  ((5:ERROR58:No config found for file bar.ml. Try calling 'dune build'.))

Request config for file in alt context after calling SetContext

  $ printf '(10:SetContext3:alt)(4:File%d:%s)' ${#FILE2} $FILE2 | dune ocaml-merlin | dune format-dune-file
  ((5:ERROR58:No config found for file bar.ml. Try calling 'dune build'.))

Request config for file in default context before and after calling SetContext

  $ printf '(4:File%d:%s)(10:SetContext3:alt)(4:File%d:%s)' ${#FILE1} $FILE1 ${#FILE1} $FILE1 | dune ocaml-merlin | dune format-dune-file
  ((6:STDLIB34:/home/me/code/dune/_opam/lib/ocaml)
   (17:EXCLUDE_QUERY_DIR)
   (1:B143:$TESTCASE_ROOT/_build/default/.foo.objs/byte)
   (1:S113:$TESTCASE_ROOT)
   (3:FLG
    (2:-w51:@1..3@5..28@31..39@43@46..47@49..57@61..62@67@69-4016:-strict-sequence15:-strict-formats12:-short-paths10:-keep-locs2:-g)))
  
  ((5:ERROR58:No config found for file foo.ml. Try calling 'dune build'.))

Combine all

  $ printf '(4:File%d:%s)(10:SetContext3:alt)(4:File%d:%s)(4:File%d:%s)' ${#FILE1} $FILE1 ${#FILE1} $FILE1 ${#FILE2} $FILE2 | dune ocaml-merlin | dune format-dune-file
  ((6:STDLIB34:/home/me/code/dune/_opam/lib/ocaml)
   (17:EXCLUDE_QUERY_DIR)
   (1:B143:$TESTCASE_ROOT/_build/default/.foo.objs/byte)
   (1:S113:$TESTCASE_ROOT)
   (3:FLG
    (2:-w51:@1..3@5..28@31..39@43@46..47@49..57@61..62@67@69-4016:-strict-sequence15:-strict-formats12:-short-paths10:-keep-locs2:-g)))
  
  ((5:ERROR58:No config found for file foo.ml. Try calling 'dune build'.))
  
  ((5:ERROR58:No config found for file bar.ml. Try calling 'dune build'.))
