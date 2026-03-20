On Windows, some packages (e.g. ocamlfind) generate .install files that list
executables without the .exe extension, but the actual built files have .exe.
opam handles this by automatically appending .exe on Windows, but Dune
currently does not. This is a regression test for
https://github.com/ocaml/dune/issues/12430

  $ make_lockdir

Create a package whose build step produces a binary with .exe extension, but
whose .install file references it without .exe (as ocamlfind does):

  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (build
  >  (run bash -c "\| cp /usr/bin/echo.exe foo.exe;
  >               "\| cat >test.install <<INSTALL
  >               "\| bin: [ "foo" {"foo"} ]
  >               "\| INSTALL
  >  ))
  > EOF

Building the package fails because Dune looks for "foo" but only "foo.exe"
exists:

  $ make_dune_project 3.22

  $ cat >dune <<EOF
  > (rule
  >  (with-stdout-to testout
  >   (run %{bin:foo} workspace-test)))
  > EOF

  $ dune build ./testout 2>&1 | strip_sandbox | sanitize_pkg_digest test.0.0.1
  Error: entry
  $SANDBOX/_private/default/.pkg/test.0.0.1-DIGEST_HASH/source/foo
  in
  $SANDBOX/_private/default/.pkg/test.0.0.1-DIGEST_HASH/source/test.install
  does not exist
  -> required by
     _build/_private/default/.pkg/test.0.0.1-DIGEST_HASH/target/cookie
  -> required by Loading all binaries in the lock directory for "default"
  -> required by looking up binary "foo" in context "default"
  -> required by %{bin:foo} at dune:3
  -> required by _build/default/testout
  [1]
