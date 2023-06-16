Test that if a package depends on dune, the presence of a file named "dune"
inside the lockdir doesn't cause dune to attempt to parse it as a regular dune
file.

  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends dune))
  > EOF

  $ dune pkg lock --opam-env=pure --opam-repository=mock-opam-repository
  Selected the following packages:
  dune.0.0.1

  $ cat dune.lock/dune
  (version 0.0.1)

  $ dune build
