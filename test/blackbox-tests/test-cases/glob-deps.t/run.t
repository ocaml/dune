  $ dune build @glob | dune_cmd sanitize
  foo/dune foo/foo$ext_lib foo/foo.cma foo/foo.cmxa foo/foo.cmxs foo/foo.ml

Globs do not match directories, so in the test below, [foo/new-file] is added
to the output but [foo/new-dir] is ignored.

  $ touch foo/new-file
  $ mkdir foo/new-dir
  $ dune build @glob | dune_cmd sanitize
  foo/dune foo/foo$ext_lib foo/foo.cma foo/foo.cmxa foo/foo.cmxs foo/foo.ml foo/new-file
