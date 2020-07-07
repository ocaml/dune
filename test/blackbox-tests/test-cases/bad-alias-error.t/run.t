  $ dune runtest --root absolute-path
  Entering directory 'absolute-path'
  File "dune", line 4, characters 9-17:
  4 |   (alias /foo/bar)))
               ^^^^^^^^
  Error: Invalid alias!
  Tried to reference path outside build dir: "/foo/bar"
  [1]
  $ dune runtest --root outside-workspace
  Entering directory 'outside-workspace'
  File "dune", line 4, characters 9-42:
  4 |   (alias %{workspace_root}/../../../foobar)))
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: path outside the workspace: ./../../../foobar from default
  [1]
