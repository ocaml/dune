A consumer of a tight-eligible local library, compiled in native
mode under [--profile release], exercises the [want_cmx = true]
branch of [Lib_file_deps.deps_of_entry_modules] — emitting
per-module [.cmx] deps in addition to per-module [.cmi] deps.

The default [dev] profile sets [opaque = true], which makes
[want_cmx = false] for local libs and the cmx branch is skipped
entirely. Switching to [release] flips [opaque] off, so the
consumer's native compile takes the per-module path with
[cm_kind = Ocaml Cmx] and the function records cmx-file deps.
Building [consumer.cmxa] explicitly forces the native compile,
unlike [@check] which only materialises [.cmi]/[.cmt] artifacts.

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > EOF

[dep_lib]: an unwrapped tight-eligible library with two modules,
one of them with both [.ml] and [.mli]:

  $ mkdir dep_lib
  $ cat > dep_lib/dune <<EOF
  > (library (name dep_lib) (wrapped false))
  > EOF
  $ cat > dep_lib/m.ml <<EOF
  > let v = 1
  > EOF
  $ cat > dep_lib/m.mli <<EOF
  > val v : int
  > EOF
  $ cat > dep_lib/n.ml <<EOF
  > let _ = ()
  > EOF

[consumer]: a library that references [M] from [dep_lib]:

  $ mkdir consumer
  $ cat > consumer/dune <<EOF
  > (library
  >   (name consumer)
  >   (libraries dep_lib))
  > EOF
  $ cat > consumer/c.ml <<EOF
  > let _ = M.v
  > EOF

Build the consumer's [.cmxa] under release profile, forcing
native compile of every module:

  $ dune build --profile release consumer/consumer.cmxa
