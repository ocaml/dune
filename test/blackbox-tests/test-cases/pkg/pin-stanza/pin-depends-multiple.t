Having multiple opam packages that all depend on one pinned pkg in
the workspace causes confusion

  $ mkrepo
  $ add_mock_repo_if_needed

  $ echo '(lang dune 1.0)' > dune-project

The trouble maker package is just a normal package
  $ mkdir trouble
  $ cat > trouble/trouble.opam << EOF
  > opam-version: "2.0"
  > build: [ "echo" "trouble" ]
  > EOF

Main left & right both depend on the trouble maker package in their opam file
  $ cat > main_left.opam << EOF
  > opam-version: "2.0"
  > build: [ "echo" "main_left" ]
  > depends: [ "trouble" ]
  > pin-depends: [ "trouble.1.0.0" "file://$PWD/trouble" ]
  > EOF

  $ cat > main_right.opam << EOF
  > opam-version: "2.0"
  > build: [ "echo" "main_right" ]
  > depends: [  "trouble" ]
  > pin-depends: [ "trouble.1.0.0" "file://$PWD/trouble" ]
  > EOF

  $ dune_pkg_lock_normalized
  File "main_left.opam", line 1, characters 0-0:
  Error: local package trouble cannot be pinned
  [1]
