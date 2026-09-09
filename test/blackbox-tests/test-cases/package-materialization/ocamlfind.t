Test that the install layout sets OCAMLPATH correctly so that ocamlfind
can locate declared packages and those reached through library dependencies.

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package (name mylib) (depends myutil))
  > (package (name myutil))
  > EOF

  $ mkdir mylib-src myutil-src

  $ cat >myutil-src/dune <<EOF
  > (library (public_name myutil))
  > EOF

  $ cat >myutil-src/myutil.ml <<EOF
  > let greeting = "Hello from myutil"
  > EOF

  $ cat >mylib-src/dune <<EOF
  > (library (public_name mylib) (libraries myutil))
  > EOF

  $ cat >mylib-src/mylib.ml <<EOF
  > let msg = Myutil.greeting ^ "!"
  > EOF

Declaring (package mylib): ocamlfind finds mylib via the layout. The
build succeeding is the observational signal that ocamlfind resolved
the query against the layout's OCAMLPATH.

  $ cat >dune <<'EOF'
  > (rule
  >  (deps (package mylib))
  >  (action
  >   (with-stdout-to out
  >    (run ocamlfind query mylib))))
  > EOF

  $ dune build out

The library dependency on myutil brings its package into the layout.

  $ cat >dune <<'EOF'
  > (rule
  >  (deps (package mylib))
  >  (action
  >   (with-stdout-to out2
  >    (run ocamlfind query myutil))))
  > EOF

  $ dune build out2

Declaring both packages explicitly also makes both visible.

  $ cat >dune <<'EOF'
  > (rule
  >  (deps (package mylib) (package myutil))
  >  (action
  >   (with-stdout-to out3
  >    (run ocamlfind query myutil))))
  > EOF

  $ dune build out3
