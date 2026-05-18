A `consumer` library that references a module of an unwrapped
local dep, compiled under `--profile release`, builds correctly
including the native-only `.cmx` artefacts. Pins the cmx-deps
code path as a regression guard — the default `dev` profile has
`opaque = true` and the native compile of consumer modules skips
the `.cmx` branch entirely; `--profile release` flips `opaque` off
so the cmx branch fires and the consumer's `.cmxa` builds.

  $ make_dune_project 3.24

`dep_lib`: an unwrapped library with two modules, one of which
has both an `.ml` and a `.mli`.

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

`consumer`: references `M` from `dep_lib`.

  $ mkdir consumer
  $ cat > consumer/dune <<EOF
  > (library
  >   (name consumer)
  >   (libraries dep_lib))
  > EOF
  $ cat > consumer/c.ml <<EOF
  > let _ = M.v
  > EOF

Build the consumer's `.cmxa` under release profile, forcing native
compile of every module.

  $ dune build --profile release consumer/consumer.cmxa

The consumer's native compile rule depends on `dep_lib`'s `.cmi`
and `.cmx` artefacts (today via globs over the byte and native
objdirs). The presence of the `.cmx` glob pins that the
`want_cmx = true` branch fires under release profile.

  $ dune rules --root . --format=json --profile release --deps '%{cmx:consumer/c}' > deps.json
  $ jq -r 'include "dune"; .[] | depsGlobs
  >   | select(.dir | endswith("dep_lib/.dep_lib.objs/byte"))
  >   | .dir + " " + .predicate' < deps.json
  _build/default/dep_lib/.dep_lib.objs/byte *.cmi
  $ jq -r 'include "dune"; .[] | depsGlobs
  >   | select(.dir | endswith("dep_lib/.dep_lib.objs/native"))
  >   | .dir + " " + .predicate' < deps.json
  _build/default/dep_lib/.dep_lib.objs/native *.cmx
  $ jq -r 'include "dune"; .[] | depsFilePaths
  >   | select(endswith("dep_lib/.dep_lib.objs/byte/m.cmi"))' < deps.json
  $ jq -r 'include "dune"; .[] | depsFilePaths
  >   | select(endswith("dep_lib/.dep_lib.objs/native/m.cmx"))' < deps.json
