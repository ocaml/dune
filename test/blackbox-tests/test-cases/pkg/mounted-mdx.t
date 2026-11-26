An mdx stanza in a project mounted from a lock directory checks the files of
its mounted directory (stanzas/mdx-stanza/mld-files.t).

  $ make_lockdir
  $ make_named_package_project main 3.25 foo
  $ mkdir -p _source/other
  $ (cd _source && make_mdx_project 3.25 0.4)
  $ cat >> _source/dune-project <<'EOF2'
  > (package (name foo))
  > (package (name bar))
  > EOF2
  $ echo '(mdx (package foo))' > _source/dune
  $ echo '(mdx (package bar))' > _source/other/dune
  $ cat > _source/README.md <<'EOF2'
  > ```ocaml
  > # 1 + 1;;
  > - : int = 3
  > ```
  > EOF2
  $ cp _source/README.md _source/other/README.md
  $ make_dune_source_lockpkg native foo "$PWD/_source"

The stanza of the locked package [foo] picks up the mounted README.md. The
mount only sees libraries from the lock directory, so the runtime libraries of
the mdx generator are not found and the check does not run.

  $ dune build @_build/default/.pkg/foo/runtest 2>&1 | grep '^Error'
  Error: Library "mdx.top" not found.
  [1]
  $ ls _build/default/.pkg/foo
  README.md
  mdx_gen.ml-gen

The stanza attached to [bar], a package of the mounted project that is not
locked, generates no rules.

  $ dune build @_build/default/.pkg/foo/other/runtest
  $ ls _build/default/.pkg/foo/other
  ls: cannot access '_build/default/.pkg/foo/other': No such file or directory
  [2]
