%{pkg:...:section} in regular rules should work with lock-file packages.

  $ make_lockdir

Create a lock-file package that installs a file into share:

  $ make_lockpkg lockpkg <<'EOF'
  > (version 0.0.1)
  > (install
  >  (progn
  >   (run mkdir -p %{pkg-self:share})
  >   (system "echo lock-data > %{pkg-self:share}/data.txt")))
  > EOF

  $ cat >dune-project <<EOF
  > (lang dune 3.23)
  > EOF

Use %{pkg:lockpkg:share} in a regular rule to reference the lock-file
package's install path:

  $ cat >dune <<EOF
  > (rule
  >  (alias test-lock-pkg-path)
  >  (action (echo "share: %{pkg:lockpkg:share}\n")))
  > EOF

  $ dune build @test-lock-pkg-path 2>&1 | sanitize_pkg_digest lockpkg.0.0.1
  share: ../_private/default/.pkg/lockpkg.0.0.1-DIGEST_HASH/target/share/lockpkg
