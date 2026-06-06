Test that the target directory exists

  $ make_melange_project 3.8 0.1

  $ cat > dune <<EOF
  > (melange.emit
  >  (alias mel)
  >  (emit_stdlib false)
  >  (target output))
  > EOF

Create the target dir

  $ mkdir ./output
  $ cat > output/dune <<EOF
  > (rule
  >  (with-stdout-to index.txt (echo "hello")))
  > (alias (name mel) (deps index.txt))
  > EOF
  $ cat > hello.ml <<EOF
  > let () = Js.log "hello"
  > EOF

  $ dune build @mel
  $ ls _build/default/output
  hello.js
  index.txt

Target promotion works

  $ dune clean
  $ cat > dune <<EOF
  > (melange.emit
  >  (alias mel)
  >  (target output)
  >  (emit_stdlib false)
  >  (promote (until-clean)))
  > EOF

  $ dune build @mel
  $ ls _build/default/output
  hello.js
  index.txt


