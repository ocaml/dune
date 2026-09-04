We should be able to specify (package ..) deps on locally built packages.

  $ make_dune_project 3.11

  $ make_lockdir
  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > (install
  >  (progn
  >   (run mkdir -p %{prefix}/bin)
  >   (run touch %{prefix}/bin/foo)
  >   (run chmod +x %{prefix}/bin/foo)))
  > EOF

  $ cat >dune <<'EOF'
  > (dirs :standard \ external_sources)
  > (rule
  >  (alias foo)
  >  (action
  >   (progn
  >    (run which foo)
  >    (echo %{bin:foo})))
  >  (deps (package foo)))
  > EOF

  $ dune build @foo 2>&1 | sanitize_pkg_digest foo.0.0.1
  $TESTCASE_ROOT/_build/_private/default/.pkg/foo.0.0.1-DIGEST_HASH/target/bin/foo
  ../_private/default/.pkg/foo.0.0.1-DIGEST_HASH/target/bin/foo

Now we define the external package using a dune project:

  $ mkdir external_sources
  $ cat >external_sources/dune-project <<EOF
  > (lang dune 3.11)
  > (package (name foo))
  > EOF
  $ cat >external_sources/dune <<EOF
  > (executable
  >  (public_name foo))
  > EOF
  $ cat >external_sources/foo.ml <<EOF
  > print_endline "Hello from foo.ml!"
  > EOF

  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/external_sources))
  > (build (run dune build @install --promote-install-files))
  > EOF
  $ dune build @foo 2>&1 | sanitize_pkg_digest foo.0.0.1
  $TESTCASE_ROOT/_build/_private/default/.pkg/foo.0.0.1-DIGEST_HASH/target/bin/foo
  ../_private/default/.pkg/foo.0.0.1-DIGEST_HASH/target/bin/foo

A dependency on a package built by Dune's package manager also depends on the
closure of the libraries provided by that package.

  $ mkdir -p $TMPDIR/package_sources/a \
  > $TMPDIR/package_sources/b/unrelated $TMPDIR/package_sources/c
  $ cat >$TMPDIR/package_sources/b/dune-project <<EOF
  > (lang dune 3.24)
  > (package (name b))
  > EOF
  $ cat >$TMPDIR/package_sources/b/dune <<EOF
  > (library (public_name b))
  > EOF
  $ cat >$TMPDIR/package_sources/b/b.ml <<EOF
  > let value = 1
  > EOF
  $ cat >$TMPDIR/package_sources/b/unrelated/dune <<EOF
  > (library
  >  (name unrelated)
  >  (public_name b.unrelated))
  > EOF
  $ cat >$TMPDIR/package_sources/b/unrelated/unrelated.ml <<EOF
  > let value = 2
  > EOF

  $ cat >$TMPDIR/package_sources/c/dune-project <<EOF
  > (lang dune 3.24)
  > (package (name c))
  > EOF
  $ cat >$TMPDIR/package_sources/c/dune <<EOF
  > (library (public_name c))
  > EOF
  $ cat >$TMPDIR/package_sources/c/c.ml <<EOF
  > let value = 3
  > EOF

  $ cat >$TMPDIR/package_sources/a/dune-project <<EOF
  > (lang dune 3.24)
  > (package
  >  (name a)
  >  (depends b c))
  > EOF
  $ cat >$TMPDIR/package_sources/a/dune <<EOF
  > (library
  >  (public_name a)
  >  (libraries b))
  > EOF
  $ cat >$TMPDIR/package_sources/a/a.ml <<EOF
  > let value = B.value + 1
  > EOF

  $ make_lockpkg b <<EOF
  > (version 0.0.1)
  > (source (copy $TMPDIR/package_sources/b))
  > (build (run dune build @install --promote-install-files))
  > EOF
  $ make_lockpkg c <<EOF
  > (version 0.0.1)
  > (source (copy $TMPDIR/package_sources/c))
  > (build (run dune build @install --promote-install-files))
  > EOF
  $ make_lockpkg a <<EOF
  > (version 0.0.1)
  > (depends b c)
  > (source (copy $TMPDIR/package_sources/a))
  > (build (run dune build @install --promote-install-files))
  > EOF

  $ cat >>dune <<'EOF'
  > (rule
  >  (target libraries)
  >  (deps (package a))
  >  (action
  >   (with-stdout-to %{target}
  >    (run ocamlfind query -recursive a))))
  > EOF

  $ dune build libraries
  $ grep -q '/lib/a$' _build/default/libraries
  $ grep -q '/lib/b$' _build/default/libraries
  $ ! grep -q '/lib/c$' _build/default/libraries

The consumer action should track the required library's installed files, not
only the package-manager build action for `a`.

  $ dune rules --format=json libraries |
  > jq_dune '.[] | ruleDepFilePaths' |
  > grep -q '/target/lib/b/b.cmi'
  $ dune rules --format=json libraries |
  > jq_dune '.[] | ruleDepFilePaths' |
  > grep -q '/target/lib/b/dune-package'
  $ ! dune rules --format=json libraries |
  > jq_dune '.[] | ruleDepFilePaths' |
  > grep -q '/target/lib/b/unrelated/unrelated.cmi'
  $ ! dune rules --format=json libraries |
  > jq_dune '.[] | ruleDepFilePaths' |
  > grep -q '/target/lib/c/c.cmi'
