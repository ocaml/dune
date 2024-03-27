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

  $ FILE=$PWD/lib3.ml
  $ printf "(11:GetContexts)" ${#FILE} $FILE | dune ocaml-merlin
  (3:alt7:default)
