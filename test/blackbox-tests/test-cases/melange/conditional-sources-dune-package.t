Show installing conditional `*.melange.ml` sources is serialized in dune-package

  $ mkdir a app
  $ cat > a/dune-project <<EOF
  > (lang dune 3.24)
  > (package (name a))
  > (using melange 0.1)
  > EOF
  $ cat > a/dune <<EOF
  > (library
  >  (modes melange :standard)
  >  (name a)
  >  (public_name a.sub))
  > EOF

  $ cat > a/foo.ml <<EOF
  > let x = "foo"
  > EOF
  $ cat > a/melange_only.melange.ml <<EOF
  > let x = "melange"
  > EOF

  $ dune build --root a
  $ dune install --root a --prefix $PWD/prefix > /dev/null

  $ grep -E "melange_modules|a__Melange_only|sub/melange_only.ml" prefix/lib/a/dune-package
     sub/melange/a__Melange_only.cmi
     sub/melange/a__Melange_only.cmj
     sub/melange/a__Melange_only.cmt
   (melange_modules
        (obj_name a__Melange_only)
        (source (path Melange_only) (impl (path sub/melange_only.ml))))))

  $ cat > app/dune-project <<EOF
  > (lang dune 3.8)
  > (using melange 0.1)
  > EOF
  $ cat > app/dune <<EOF
  > (melange.emit
  >  (target dist)
  >  (alias dist)
  >  (emit_stdlib false)
  >  (libraries a.sub))
  > EOF
  $ cat > app/bar.ml <<EOF
  > let () = Js.log A.Melange_only.x
  > EOF

  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build --root app @dist
  $ find app/_build/default/dist/node_modules/a.sub -type f | sort
  app/_build/default/dist/node_modules/a.sub/a.js
  app/_build/default/dist/node_modules/a.sub/foo.js
  app/_build/default/dist/node_modules/a.sub/melange_only.js
  $ node app/_build/default/dist/bar.js
  melange
