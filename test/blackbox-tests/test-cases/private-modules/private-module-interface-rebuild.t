Changing a private module's interface does not rebuild consumers (#16071).

  $ make_dune_project 3.24

  $ mkdir dep
  $ cat > dep/dune <<EOF
  > (library
  >  (name dep)
  >  (private_modules priv))
  > EOF
  $ cat > dep/priv.ml <<EOF
  > let v = 1
  > EOF
  $ cat > dep/priv.mli <<EOF
  > val v : int
  > EOF
  $ cat > dep/pub.ml <<EOF
  > module P = Priv
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name main)
  >  (libraries dep))
  > EOF
  $ cat > main.ml <<EOF
  > let () = print_int Dep.Pub.P.v
  > EOF

  $ dune build ./main.exe

Changing the private interface should rebuild [main]. Instead, its stale [.cmx]
causes linking to fail.

  $ cat > dep/priv.ml <<EOF
  > let v = 2
  > let w = 3
  > EOF
  $ cat > dep/priv.mli <<EOF
  > val v : int
  > val w : int
  > EOF
  $ dune build ./main.exe
  File "_none_", line 1:
  Error: Files .main.eobjs/native/dune__exe__Main.cmx and dep/dep.cmxa
         make inconsistent assumptions over interface Dep__Priv
  [1]
