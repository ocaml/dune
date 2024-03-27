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

  $ FILE=$PWD/lib3.ml
  $ printf '(4:File%d:%s)(10:SetContext3:alt)(4:File%d:%s)(4:File%d:%s)' ${#FILE} $FILE | dune ocaml-merlin
  ((5:ERROR59:No config found for file lib3.ml. Try calling 'dune build'.))((5:ERROR53:No config found for file .. Try calling 'dune build'.))((5:ERROR53:No config found for file .. Try calling 'dune build'.))
