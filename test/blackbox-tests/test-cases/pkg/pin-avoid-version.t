Pinned packages ignore the avoid-version flag. This is intentional: a pin
selects a single version, so there's no other version to fallback to and
avoiding it would only make the solve fail.

  $ mkrepo
  $ add_mock_repo_if_needed
  $ make_dune_project 3.13
  $ mkpkg fallback

The pinned package is flagged with avoid-version:

  $ mkdir _pinned
  $ cat >_pinned/pinned.opam <<EOF
  > opam-version: "2.0"
  > flags: [avoid-version]
  > EOF

The local package depends on either the pinned package or on a regular package
coming from the repository:

  $ cat >root.opam <<EOF
  > opam-version: "2.0"
  > depends: [ "pinned" | "fallback" ]
  > pin-depends: [ "pinned.1.0.0" "file://$PWD/_pinned" ]
  > EOF

The solver selects the pinned package, despite its avoid-version flag:

  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  - pinned.1.0.0 (this version should be avoided)
