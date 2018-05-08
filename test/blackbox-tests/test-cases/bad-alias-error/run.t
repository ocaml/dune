  $ dune runtest --root absolute-path 2>&1 | grep -v Entering
  Invalid alias!
  Tried to reference alias "/foo/bar"
  $ dune runtest --root outside-workspace 2>&1 | grep -v Entering
  Path outside the workspace: ./../../../foobar from _build/default
