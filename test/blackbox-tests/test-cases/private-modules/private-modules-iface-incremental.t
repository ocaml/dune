Regression test for https://github.com/ocaml/dune/issues/16071.

Editing the interface (.mli) of a [(private_modules ...)] module must trigger a
rebuild of consumers that transitively read it through [-H] hidden includes
(OCaml >= 5.2).  Before the fix, the .cmi dep-set glob only covered
[public_cmi_ocaml_dir] and missed [byte_dir] where private .cmi files live, so
the interface change was invisible to Dune's rebuild logic and the subsequent
build would fail with "inconsistent assumptions over interface".

  $ make_dune_project 3.24

[dep] is a library with one public module [Pub] and one private module [Priv];
[Pub] re-exports [Priv] as a submodule so that the executable can observe
[Priv]'s interface:

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

[main] uses [Dep.Pub.P.v]:

  $ cat > dune <<EOF
  > (executable (name main) (libraries dep))
  > EOF
  $ cat > main.ml <<EOF
  > let () = print_int Dep.Pub.P.v
  > EOF

Initial build succeeds:

  $ dune build ./main.exe
  $ ./_build/default/main.exe
  1

Now extend [priv.mli] with a new value [w] and update [priv.ml] accordingly.
The rebuild must succeed — the consumer must be recompiled against the new
interface:

  $ cat > dep/priv.mli <<EOF
  > val v : int
  > val w : int
  > EOF
  $ cat > dep/priv.ml <<EOF
  > let v = 2
  > let w = 3
  > EOF
  $ dune build ./main.exe
  $ ./_build/default/main.exe
  2
