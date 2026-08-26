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

The logical module artifact reference has four components:

  $ dune build '%{cmi:Foo.Foo.Foo.Foo}'

A five-component source path must not silently resolve the shallow [Foo]
group alias:

  $ dune build '%{cmi:foo/foo/foo/foo/foo}'
  File "command line", line 1, characters 0-26:
  Error: Module reference Foo does not match the module at this source path.
  Hint: Foo.Foo.Foo.Foo would be a correct module reference
  [1]

The installed format keeps the group-interface component that distinguishes
the source-trie path from the four-component logical module path.

  $ dune build @install
  $ grep -o '(path Foo Foo Foo Foo Foo)' \
  > _build/install/default/lib/repeated/dune-package
  (path Foo Foo Foo Foo Foo)
