A lock directory which does not exist in the source tree:

  $ mkdir project

  $ make_lockdir
  $ make_lockpkg foo <<EOF
  > (build (run echo foo))
  > (version 1.0.0)
  > EOF

  $ cat >dune-workspace <<EOF
  > (lang dune 3.13)
  > (lock_dir (path $PWD/dune.lock))
  > EOF

  $ build_pkg foo 2>&1 | awk '/Internal error/,/Raised/'
  Internal error! Please report to https://github.com/ocaml/dune/issues,
  providing the file _build/trace.csexp, if possible. This includes build
  commands, message logs, and file paths.
  Description:
    ("Local.relative: received absolute path",
     { t = "."
     ; path =
         "$TESTCASE_ROOT/dune.lock"
     })
  Raised at Stdune__Code_error.raise in file
  [1]
