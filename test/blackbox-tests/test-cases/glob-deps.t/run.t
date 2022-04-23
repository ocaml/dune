  $ dune build @glob | dune_cmd sanitize
  foo/dune foo/foo$ext_lib foo/foo.cma foo/foo.cmxa foo/foo.cmxs foo/foo.ml

Globs do not match directories, so in the test below, [foo/new-file] is added
to the output but [foo/new-dir] is ignored.

  $ touch foo/new-file
  $ mkdir foo/new-dir
  $ dune build @glob | dune_cmd sanitize
  foo/dune foo/foo$ext_lib foo/foo.cma foo/foo.cmxa foo/foo.cmxs foo/foo.ml foo/new-file

Globs should not match directories when matching in external folders either:

  $ DIR=glob-external-dir
  $ mkdir $DIR
  $ cd $DIR
  $ cat >dune-project <<EOF
  > (lang dune 3.2)
  > EOF
  $ mkdir -p _foo/dir
  $ touch _foo/file
  $ cat >dune <<EOF
  > (rule
  >  (alias foo)
  >  (deps (glob_files $PWD/_foo/*))
  >  (action (echo %{deps})))
  > EOF
  $ dune build @foo
  $TESTCASE_ROOT/glob-external-dir/_foo/file
