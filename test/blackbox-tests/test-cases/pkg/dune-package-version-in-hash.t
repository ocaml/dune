Check if the dune binary version or digest is included in the package hash

  $ cat >dune-project <<EOF
  > (lang dune 3.22)
  > (package
  >  (name test)
  >  (allow_empty)
  >  (depends mypkg))
  > EOF
  $ make_lockdir

Use an env var to control the dune-package version written by the build:

  $ make_lockpkg mypkg <<'EOF'
  > (version 0.0.1)
  > (build
  >  (progn
  >   (run mkdir -p %{lib}/%{pkg-self:name})
  >   (run sh -c "\
  >     printf '(lang dune %s)\n(name mypkg)\n(use_meta)\n' \
  >       $DUNE_PACKAGE_LANG > %{lib}/%{pkg-self:name}/dune-package")
  >   (run touch %{lib}/%{pkg-self:name}/META)))
  > EOF

Build with a valid version:

  $ DUNE_PACKAGE_LANG=3.22 build_pkg mypkg
  $ cat "$(get_build_pkg_dir mypkg)/target/lib/mypkg/dune-package" | grep "lang"
  (lang dune 3.22)
  $ dune pkg print-digest mypkg > hash1.txt

Now rebuild with version 99.0 (simulating a newer dune having built this):

  $ dune clean
  $ DUNE_PACKAGE_LANG=99.0 build_pkg mypkg
  $ cat "$(get_build_pkg_dir mypkg)/target/lib/mypkg/dune-package" | grep "lang"
  (lang dune 99.0)
  $ dune pkg print-digest mypkg > hash2.txt

The hash is unchanged despite the different dune-package version:

  $ diff hash1.txt hash2.txt
  $ cat hash1.txt | sanitize_pkg_digest mypkg.0.0.1
  mypkg.0.0.1-DIGEST_HASH

But dune can't read the package anymore:

  $ cat >dune <<EOF
  > (executable (name main) (libraries mypkg))
  > EOF
  $ cat >main.ml <<EOF
  > let () = ()
  > EOF
  $ dune build main.exe 2>&1 | sanitize_pkg_digest mypkg.0.0.1 | head -5
  File "_build/_private/default/.pkg/mypkg.0.0.1-DIGEST_HASH/target/lib/mypkg/dune-package", line 1, characters 11-15:
  1 | (lang dune 99.0)
                 ^^^^
  Error: Version 99.0 of the dune language is not supported.
  Supported versions of this extension in version 99.0 of the dune language:
  [1]
