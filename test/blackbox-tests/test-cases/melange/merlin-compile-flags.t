Show that the merlin config knows about melange.compile_flags

  $ make_melange_project 3.8 0.1

  $ lib=foo
  $ cat >dune <<EOF
  > (library
  >  (name $lib)
  >  (private_modules bar)
  >  (melange.compile_flags :standard -w +42)
  >  (modes melange))
  > EOF

  $ touch bar.ml $lib.ml
  $ dune build @check

  $ dune ocaml merlin dump-config --format=json "$PWD" | jq_dune -r '
  > merlinEntry("Foo")
  > | merlinConfigItemsNamed(["FLG"])
  > | select(.[0] == "FLG" and (.[1] | index("+42")))
  > | @json'
  ["FLG",["-w","@1..3@5..28@30..39@43@46..47@49..57@61..62@67@69-40","-strict-sequence","-strict-formats","-short-paths","-keep-locs","-g","-w","+42"]]
  ["FLG",["-w","@1..3@5..28@30..39@43@46..47@49..57@61..62@67@69-40","-strict-sequence","-strict-formats","-short-paths","-keep-locs","-g","-w","+42"]]

  $ cat >dune <<EOF
  > (melange.emit
  >  (target output)
  >  (emit_stdlib false)
  >  (compile_flags :standard -w +42 ))
  > EOF

  $ dune build @check

  $ dune ocaml merlin dump-config --format=json "$PWD" | jq_dune -r '
  > merlinEntry("Melange")
  > | merlinConfigItemsNamed(["FLG"])
  > | select(.[0] == "FLG" and (.[1] | index("+42")))
  > | @json'
  ["FLG",["-w","@1..3@5..28@30..39@43@46..47@49..57@61..62@67@69-40","-strict-sequence","-strict-formats","-short-paths","-keep-locs","-g","-w","+42"]]
  ["FLG",["-w","@1..3@5..28@30..39@43@46..47@49..57@61..62@67@69-40","-strict-sequence","-strict-formats","-short-paths","-keep-locs","-g","-w","+42"]]
