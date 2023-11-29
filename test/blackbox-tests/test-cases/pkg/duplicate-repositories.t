Duplicate repository definition in the same workspace file:

  $ cat >dune-workspace <<EOF
  > (lang dune 3.11)
  > (repository
  >  (name foo)
  >  (source "git+file//$PWD/foo"))
  > (repository
  >  (name foo)
  >  (source "git+file//$PWD/foo"))
  > EOF

  $ dune pkg outdated 2>&1 | awk '/Internal error/,/Raised/'
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("Map.of_list_exn", { key = "foo" })
  Raised at Stdune__Code_error.raise in file
