Test that the install layout sets OCAMLPATH correctly so that ocamlfind
can locate packages in the layout.

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

ocamlfind can locate mylib via the layout OCAMLPATH:

  $ cat >dune <<'EOF'
  > (rule
  >  (deps (package mylib))
  >  (action
  >   (with-stdout-to out
  >    (run ocamlfind query mylib))))
  > EOF

  $ dune build out
  $ grep -q '.install-layout.*lib/mylib' _build/default/out
  [1]

The transitive dep myutil is also findable (included via closure):

  $ cat >dune <<'EOF'
  > (rule
  >  (deps (package mylib))
  >  (action
  >   (with-stdout-to out2
  >    (run ocamlfind query myutil))))
  > EOF

  $ dune build out2
  File "dune", lines 1-5, characters 0-96:
  1 | (rule
  2 |  (deps (package mylib))
  3 |  (action
  4 |   (with-stdout-to out2
  5 |    (run ocamlfind query myutil))))
  ocamlfind: Package `myutil' not found
  [1]
  $ grep -q '.install-layout.*lib/myutil' _build/default/out2
  grep: _build/default/out2: No such file or directory
  [2]
