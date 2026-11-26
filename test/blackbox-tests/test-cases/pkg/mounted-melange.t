A melange.emit stanza in a project mounted from a lock directory emits the
JavaScript of its mounted modules and libraries (melange/emit-private.t).

  $ make_lockdir
  $ make_named_package_project main 3.25 foo
  $ mkdir -p _source/lib
  $ (cd _source && make_melange_project 3.25 1.0)
  $ echo '(package (name foo))' >> _source/dune-project
  $ cat > _source/lib/dune <<'EOF2'
  > (library (public_name foo.lib) (name lib) (modes melange))
  > EOF2
  $ echo 'let value = 42' > _source/lib/lib.ml
  $ cat > _source/dune <<'EOF2'
  > (melange.emit
  >  (target out)
  >  (emit_stdlib false)
  >  (libraries lib))
  > EOF2
  $ cat > _source/main.ml <<'EOF2'
  > let () = Js.log Lib.value
  > EOF2
  $ make_dune_source_lockpkg native foo "$PWD/_source"

As for a stanza in a workspace subdirectory, the output tree repeats the
directory of the stanza in the context, here the mount.

  $ dune build @_build/default/.pkg/foo/melange
  $ find _build/default/.pkg/foo/out -name '*.js' | sort
  _build/default/.pkg/foo/out/.pkg/foo/main.js
  _build/default/.pkg/foo/out/node_modules/foo.lib/lib.js
  $ node _build/default/.pkg/foo/out/.pkg/foo/main.js
  42

Promoting the output of a mounted stanza that emits a public library is
refused.

  $ cat > _source/dune <<'EOF2'
  > (melange.emit
  >  (target out)
  >  (emit_stdlib false)
  >  (promote (until-clean))
  >  (libraries lib))
  > EOF2
  $ dune build @_build/default/.pkg/foo/melange
  File "_build/default/.pkg/foo", line 1, characters 0-0:
  Error: Promotion is not supported for mounted package sources.
  [1]
