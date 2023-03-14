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
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("Install.Paths.get", {})
  Raised at Stdune__Code_error.raise in file
