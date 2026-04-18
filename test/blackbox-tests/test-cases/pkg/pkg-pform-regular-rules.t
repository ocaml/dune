%{pkg:package:section:file} in regular rules should work with lock-file
packages, resolving to the file in the package's target directory.

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

  $ cat >dune <<EOF
  > (rule
  >  (alias test-lock-pkg-file)
  >  (action (echo "%{pkg:lockpkg:share:data.txt}\n")))
  > EOF

  $ dune build @test-lock-pkg-file 2>&1 | sanitize_pkg_digest lockpkg.0.0.1
  ../_private/default/.pkg/lockpkg.0.0.1-DIGEST_HASH/target/share/lockpkg/data.txt
