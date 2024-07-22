Test that it's possible to lock a project that depends on a pinned
package with depopts.

  $ . ../helpers.sh
  $ mkrepo
  $ add_mock_repo_if_needed

  $ mkdir pkg-with-depopts
  $ cat > pkg-with-depopts/pkg-with-depopts.opam <<EOF
  > opam-version: "2.0"
  > depopts: [
  >  "option-a"
  >  "option-b"
  > ]
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > (pin
  >  (url "file://$PWD/pkg-with-depopts")
  >  (package
  >   (name pkg-with-depopts)
  >   (version 5.2.0)))
  > (package
  >  (name foo)
  >  (depends pkg-with-depopts))
  > EOF

  $ dune pkg lock
  Solution for dune.lock:
  - pkg-with-depopts.5.2.0

Try locking the project again after adding a bare dune-project file to
the pinned package. Currently this doesn't work correctly. See
https://github.com/ocaml/dune/issues/10739
  $ cat > pkg-with-depopts/dune-project <<EOF
  > (lang dune 3.16)
  > EOF
  $ dune pkg lock
  File "pkg-with-depopts/pkg-with-depopts.opam", line 1, characters 0-0:
  Error: Expected formula to be a conjunction of atoms but encountered non-atom
  term 'option-a | option-b'
  [1]
