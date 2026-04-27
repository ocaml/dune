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
  > (lang dune 3.24)
  > EOF

The rule depends on the lock-file package cookie and the resolved file:

  $ cat >dune <<EOF
  > (rule
  >  (target output)
  >  (action (with-stdout-to %{target} (echo %{pkg:lockpkg:share:data.txt}))))
  > EOF

CR-someday alizter: dune rules --deps requires a prior build because
Install_cookie.load_exn runs eagerly during dep evaluation.

  $ dune build _build/default/output 2>&1

  $ dune rules --deps _build/default/output 2>&1 | sanitize_pkg_digest lockpkg.0.0.1
  ((File
    (In_build_dir
     _build/_private/default/.pkg/lockpkg.0.0.1-DIGEST_HASH/target/cookie))
   (File
    (In_build_dir
     _build/_private/default/.pkg/lockpkg.0.0.1-DIGEST_HASH/target/share/lockpkg/data.txt)))

Requesting a file that doesn't exist in the lock-file package is rejected:

  $ cat >dune <<EOF
  > (rule
  >  (alias test-missing)
  >  (action (echo "%{pkg:lockpkg:share:missing.txt}\n")))
  > EOF

  $ dune build @test-missing 2>&1
  File "dune", line 3, characters 16-48:
  3 |  (action (echo "%{pkg:lockpkg:share:missing.txt}\n")))
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: File missing.txt not found in section share of package lockpkg
  [1]
