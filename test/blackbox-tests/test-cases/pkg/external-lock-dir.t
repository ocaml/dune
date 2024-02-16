A lock directory which does not exist in the source tree:

  $ . ./helpers.sh

  $ mkdir dune.lock project
  $ cat >dune.lock/foo.pkg <<EOF
  > (build (run echo foo))
  > (version 1.0.0)
  > EOF

  $ cat >dune-workspace <<EOF
  > (lang dune 3.13)
  > (lock_dir (path $PWD/dune.lock))
  > EOF

  $ build_pkg foo 2>&1 | awk '/Internal error/,/Raised/'
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("Local.relative: received absolute path",
    { t = "."
    ; path =
        "$TESTCASE_ROOT/dune.lock"
    })
  Raised at Stdune__Code_error.raise in file
