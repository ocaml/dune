Reproduce #8281

Whenever an invalid package name is used, dune crashes when building @doc

  $ cat >dune-project <<EOF
  > (lang dune 2.4)
  > EOF
  $ touch x.opam x.y.opam

  $ mkdir x && cd x
  $ cat >dune <<EOF
  > (library
  >  (public_name x))
  > EOF

  $ mkdir y && cd y
  $ cat >dune <<EOF
  > (library
  >  (public_name x.y)
  >  (name x_y))
  > EOF
  $ cd ..

  $ cd ..

  $ dune build @doc 2>&1 | awk '/Internal error/,/Raised/'
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("[gen_rules] returned rules in a directory that is not a descendant of the directory it was called for",
    { dir = In_build_dir "default/_doc/_html/x.y"
    ; example =
        Rule
          { targets =
              { root = In_build_dir "default/_doc/_html/x"
              ; files = set { "db.js" }
              ; dirs = set {}
              }
          }
    })
  Raised at Stdune__Code_error.raise in file
