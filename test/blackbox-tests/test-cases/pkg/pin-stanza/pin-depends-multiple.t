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

This should work
  $ dune_pkg_lock_normalized
  File "main_left.opam", line 1, characters 0-0:
  Error: local package trouble cannot be pinned
  [1]

If the versions disagree, we don't give a clear error
  $ cat > main_left.opam << EOF
  > opam-version: "2.0"
  > build: [ "echo" "main_left" ]
  > depends: [ "trouble" ]
  > pin-depends: [ "trouble.9.9.9" "file://$PWD/trouble" ]
  > EOF

  $ dune_pkg_lock_normalized
  File "main_left.opam", line 1, characters 0-0:
  Error: local package trouble cannot be pinned
  [1]

If the urls disagree, we also don't give a clear error
  $ cat > main_left.opam << EOF
  > opam-version: "2.0"
  > build: [ "echo" "main_left" ]
  > depends: [ "trouble" ]
  > pin-depends: [ "trouble.1.0.0" "file://$PWD/other" ]
  > EOF

  $ dune_pkg_lock_normalized
  File "main_left.opam", line 1, characters 0-0:
  Error: local package trouble cannot be pinned
  [1]

If instead of two opam pins we have one opam pin and one dune pin
  $ cat > main_left.opam << EOF
  > opam-version: "2.0"
  > build: [ "echo" "main_left" ]
  > depends: [ "trouble" ]
  > pin-depends: [ "trouble.1.0.0" "file://$PWD/trouble" ]
  > EOF

  $ rm main_right.opam
  $ mkdir right
  $ cat > right/dune-project << EOF
  > (lang dune 3.14)
  > (pin
  >  (url "file://$PWD/trouble")
  >  (package
  >   (name trouble)
  >   (version "1.0.0")))
  > (package
  >  (name main_right)
  >  (depends trouble))
  > EOF

This works!
  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  - trouble.1.0.0

If the versions disagree, we favor the opam info?
  $ cat > main_left.opam << EOF
  > opam-version: "2.0"
  > build: [ "echo" "main_left" ]
  > depends: [ "trouble" ]
  > pin-depends: [ "trouble.0.0.1" "file://$PWD/trouble" ]
  > EOF

We pick the opam version here. 
  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  - trouble.0.0.1

If the urls disagree, we give a somewhat clear error
  $ cat > main_left.opam << EOF
  > opam-version: "2.0"
  > build: [ "echo" "main_left" ]
  > depends: [ "trouble" ]
  > pin-depends: [ "trouble.1.0.0" "file://$PWD/other" ]
  > EOF

  $ dune_pkg_lock_normalized
  File "main_left.opam", line 1, characters 0-0:
  Error: unable to discover an opam file for package trouble
  [1]

For a test covering the case where we have two pins in dune-projects,
check override-single-workspace.t
