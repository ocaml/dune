  $ dune runtest --root absolute-path 2>&1 | grep -v Entering
  File "jbuild", line 3, characters 16-24:
  Error: Invalid alias!
  Tried to reference path outside build dir: "/foo/bar"
  $ dune runtest --root outside-workspace 2>&1 | grep -v Entering
  File "jbuild", line 4, characters 16-39:
  Error: path outside the workspace: ./../../../foobar from _build/default
