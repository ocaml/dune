Qualified module groups can repeat the same name at arbitrary depths.

  $ mkdir -p foo/foo/foo/foo
  $ cat >dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name repeated))
  > EOF
  $ cat >dune <<'EOF'
  > (include_subdirs qualified)
  > (library
  >  (name repeated)
  >  (public_name repeated)
  >  (wrapped false))
  > EOF
  $ touch anchor.ml
  $ cat >foo/foo/foo/foo/foo.ml <<'EOF'
  > let value = "deep"
  > EOF

  $ dune build @check

The dotted logical module artifact reference is not yet recognized. The
five-component source path still identifies the module:

  $ dune build '%{cmi:Foo.Foo.Foo.Foo}'
  File "command line", line 1, characters 0-22:
  Error: Module Foo.Foo.Foo.Foo does not exist.
  [1]
  $ dune build '%{cmi:foo/foo/foo/foo/foo}'

The installed format keeps the group-interface component that distinguishes
the source-trie path from the four-component logical module path.

  $ dune build @install
  $ grep -o '(path Foo Foo Foo Foo Foo)' \
  > _build/install/default/lib/repeated/dune-package
  (path Foo Foo Foo Foo Foo)
