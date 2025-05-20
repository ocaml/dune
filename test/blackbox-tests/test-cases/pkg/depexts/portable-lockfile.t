Makes sure that the depext mechanism works with portable lockfiles

  $ . ../helpers.sh
  $ mkrepo
  $ add_mock_repo_if_needed

Create package that has depexts. Normally these would be conf-* packages in
opam-repository but depexts are supported on any kind of package.

All depexts resolve to the same hypothetical package name for reproducability.

  $ mkpkg foo <<EOF
  > build: [ ["true"] ]
  > depexts: [
  >   ["dev-foo"] {os-family = "debian"}
  >   ["dev-foo"] {os-distribution = "arch"}
  >   ["dev-foo"] {os-distribution = "fedora"}
  >   ["dev-foo"] {os-distribution = "centos"}
  >   ["dev-foo"] {os-distribution = "mageia"}
  >   ["dev-foo"] {os-distribution = "rhel"}
  >   ["dev-foo"] {os-distribution = "ol"}
  >   ["dev-foo"] {os-distribution = "alpine"}
  >   ["dev-foo"] {os-distribution = "nixos"}
  >   ["dev-foo"] {os = "openbsd"}
  >   ["dev-foo"] {os = "macos" & os-distribution = "homebrew"}
  >   ["dev-foo"] {os = "freebsd"}
  >   ["dev-foo"] {os = "win32" & os-distribution = "cygwinports"}
  >   ["dev-foo"] {os = "cygwin"}
  > ]
  > EOF

Create a project that uses this package

  $ cat > dune-project <<EOF
  > (lang dune 3.18)
  > (package
  >  (name x)
  >  (depends foo))
  > EOF

Locking should work and output us the name of the depext.

  $ dune pkg lock
  Solution for dune.lock:
  - foo.0.0.1
  $ dune show depext
  dev-foo

Enabling portable lock files should work

  $ export DUNE_CONFIG__PORTABLE_LOCK_DIR=enabled
  $ dune pkg lock
  Solution for dune.lock:
  - foo.0.0.1


Asking for the depexts should resolve to our platform and print the same depext
as above.

  $ dune show depext
  
 
It currently does not and this is a bug, #11824.
