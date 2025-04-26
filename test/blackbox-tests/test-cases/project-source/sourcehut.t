Test the sourcehut source type in project files.

  $ cat >dune-project <<EOF
  > (lang dune 3.1)
  > (name foo)
  > (generate_opam_files true)
  > (source (sourcehut john/doe))
  > (package
  >  (allow_empty)
  >  (name foo))
  > EOF

  $ dune build
  $ cat foo.opam | grep -i sr.ht
  homepage: "https://sr.ht/~john/doe"
  bug-reports: "https://todo.sr.ht/~john/doe"
  dev-repo: "git+https://git.sr.ht/~john/doe"
