An unwrapped library with [private_modules] forces dune to use a
dedicated public cmi directory: public modules' cmis live in a
separate [.objs/public_cmi] directory, produced by a copy rule
from the internal object dir. Per-module tight deps must emit
deps on the *public* cmi path, not the internal one — otherwise
the produce-public-cmi rule doesn't run and sandboxed compiles
fail to find the cmi.

This test exercises the [Obj_dir.Module.cm_public_file] path in
[Lib_file_deps.deps_of_entry_modules].

See: https://github.com/ocaml/dune/issues/4572

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF

[dep]: unwrapped library with one public entry module [Pub] and
one private module [Priv]. Consumers can see [Pub] via [-I]
search but not [Priv]:

  $ mkdir dep
  $ cat > dep/dune <<EOF
  > (library
  >  (name dep)
  >  (wrapped false)
  >  (private_modules priv))
  > EOF
  $ cat > dep/pub.ml <<EOF
  > let greeting () = Priv.helper ^ "!"
  > EOF
  $ cat > dep/priv.ml <<EOF
  > let helper = "hi"
  > EOF

[consumer]: references [Pub] from [dep]:

  $ mkdir consumer
  $ cat > consumer/dune <<EOF
  > (library (name consumer) (wrapped false) (libraries dep))
  > EOF
  $ cat > consumer/c.ml <<EOF
  > let v = Pub.greeting ()
  > EOF
  $ cat > consumer/d.ml <<EOF
  > let _ = ()
  > EOF

  $ dune build @check
