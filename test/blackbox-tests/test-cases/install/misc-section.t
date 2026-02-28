The misc install section isn't supported:

  $ cat >dune-project <<EOF
  > (lang dune 3.7)
  > (package
  >  (name xxx))
  > EOF

  $ cat >dune <<EOF
  > (install
  >  (section misc)
  >  (files foo))
  > EOF

  $ dune build xxx.install 2>&1 | awk '/Internal error/,/Raised/'
  Internal error! Please report to https://github.com/ocaml/dune/issues,
  providing the file _build/trace.csexp, if possible. This includes build
  commands, message logs, and file paths.
  Description:
    ("Install.Paths.get", {})
  Raised at Stdune__Code_error.raise in file
  [1]
