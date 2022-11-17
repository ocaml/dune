Test melange.emit promotion

  $ cat > dune-project <<EOF
  > (lang dune 3.6)
  > (using melange 0.1)
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (alias dist)
  >  (entries hello)
  >  (promote until-clean)
  >  (target dist)
  >  (module_system commonjs))
  > EOF

  $ cat > hello.ml <<EOF
  > let () =
  >   print_endline "hello"
  > EOF

  $ dune build @dist --display=short
          melc .dist.mobjs/melange/melange__Hello.{cmi,cmj,cmt}
          melc dist/hello.js
  $ node ./dist/hello.js
  hello


