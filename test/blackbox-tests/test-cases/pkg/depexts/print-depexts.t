 When a package fails to build, dune will print opam depexts warning.

  $ . ../helpers.sh
  $ mkrepo
  $ add_mock_repo_if_needed

Make a project :
  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name bar)
  >  (depends foo))
  > EOF
  $ cat > dune <<EOF
  > (executable
  >  (public_name bar))
  > EOF

Make dune.lock file with some depexts.
  $ make_lockdir
  $ cat > dune.lock/foo.pkg <<EOF
  > (version 0.0.1)
  > (depexts unzip gnupg)
  > EOF

Print the available depexts
  $ dune show depexts
  unzip
  gnupg
