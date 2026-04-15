Test that %{pkg:...} in regular rules introduces a dependency on the
lock-file package.

  $ make_lockdir

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

The path resolves and a dependency on the lock-file package cookie is
registered:

  $ cat >dune <<EOF
  > (rule
  >  (target output)
  >  (action (with-stdout-to %{target} (echo %{pkg:lockpkg:share}))))
  > EOF

CR-someday alizter: [dune rules --deps] should be able to show deps without
requiring a prior build. Currently it fails to load the cookie file if the
package hasn't been built yet.

  $ dune build _build/default/output 2>&1

  $ dune rules --deps _build/default/output 2>&1 | sanitize_pkg_digest lockpkg.0.0.1
  ((File
    (In_build_dir
     _build/_private/default/.pkg/lockpkg.0.0.1-DIGEST_HASH/target/cookie)))
