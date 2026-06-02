Test `melange.runtime_deps` in a library that has been installed

  $ mkdir lib app prefix
  $ cat > lib/dune-project <<EOF
  > (lang dune 3.20)
  > (package (name foo))
  > (using directory-targets 0.1)
  > (using melange 1.0)
  > EOF

  $ echo 'hello' > lib/file.txt
  $ write_melange_dir_target_runtime_deps_lib \
  >   lib \
  >   "./some_dir ./file.txt" \
  >   "./index.txt"

  $ dune build
  $ dune install --prefix $PWD/prefix --display short
  Installing $TESTCASE_ROOT/prefix/lib/foo/META
  Installing $TESTCASE_ROOT/prefix/lib/foo/dune-package
  Installing $TESTCASE_ROOT/prefix/lib/foo/file.txt
  Installing $TESTCASE_ROOT/prefix/lib/foo/melange/foo.cmi
  Installing $TESTCASE_ROOT/prefix/lib/foo/melange/foo.cmj
  Installing $TESTCASE_ROOT/prefix/lib/foo/melange/foo.cmt
  Installing $TESTCASE_ROOT/prefix/lib/foo/melange/foo.ml
  Installing $TESTCASE_ROOT/prefix/lib/foo/some_dir/inside-dir-target.txt

  $ rm -rf $PWD/prefix
  $ dune clean
  $ write_melange_dir_target_runtime_deps_lib \
  >   lib \
  >   "./some_dir/inside-dir-target.txt ./some_dir ./file.txt" \
  >   "./index.txt"

  $ dune build
  $ dune install --prefix $PWD/prefix --display short
  Installing $TESTCASE_ROOT/prefix/lib/foo/META
  Installing $TESTCASE_ROOT/prefix/lib/foo/dune-package
  Installing $TESTCASE_ROOT/prefix/lib/foo/file.txt
  Installing $TESTCASE_ROOT/prefix/lib/foo/melange/foo.cmi
  Installing $TESTCASE_ROOT/prefix/lib/foo/melange/foo.cmj
  Installing $TESTCASE_ROOT/prefix/lib/foo/melange/foo.cmt
  Installing $TESTCASE_ROOT/prefix/lib/foo/melange/foo.ml
  Installing $TESTCASE_ROOT/prefix/lib/foo/some_dir/inside-dir-target.txt
