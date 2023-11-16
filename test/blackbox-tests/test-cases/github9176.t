We check that `version = ""` in META files are correctly handled.

  $ export OPAM_SWITCH_PREFIX=`pwd`/opam-switch
  $ mkdir -p $OPAM_SWITCH_PREFIX/lib/mylib

  $ cat > $OPAM_SWITCH_PREFIX/lib/mylib/META << EOF
  > version=""
  > EOF

  $ cat > dune-project << EOF
  > (lang dune 1.0)
  > EOF

  $ cat > dune << EOF
  > (executable
  >  (name e)
  >  (libraries mylib))
  > EOF

  $ touch e.ml

  $ dune build 2>&1 | head -n 4
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("Invalid Package_version.t", { s = "" })
  Raised at Stdune__Code_error.raise in file
