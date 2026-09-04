Qualified module groups can repeat the same name at arbitrary depths.

  $ mkdir -p foo/foo/foo/foo
  $ cat >dune-project <<'EOF'
  > (lang dune 3.24)
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

The installed format keeps the group-interface component that distinguishes
the source-trie path from the four-component logical module path.

  $ dune build @install
  $ grep -o '(path Foo Foo Foo Foo Foo)' \
  > _build/install/default/lib/repeated/dune-package
  (path Foo Foo Foo Foo Foo)
