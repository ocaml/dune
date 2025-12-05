When solving portable lockdirs, if any packages in the solution are marked
avoid-version, include a message to that extent in the output.

  $ . ../helpers.sh
  $ mkrepo
  $ add_mock_repo_if_needed

  $ mkpkg foo <<EOF
  > conflicts: [ "a1" ]
  > flags: [avoid-version]
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.18)
  > (package
  >  (name x)
  >  (depends foo))
  > EOF

  $ DUNE_CONFIG__PORTABLE_LOCK_DIR=enabled dune pkg lock
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - foo.0.0.1 (this version should be avoided)
