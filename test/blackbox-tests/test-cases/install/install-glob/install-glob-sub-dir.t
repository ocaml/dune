Make sure we can handle globs in dune files inside subdirectories

  $ cat >dune-project <<EOF
  > (lang dune 3.6)
  > (package (name foo))
  > EOF

  $ mkdir -p sub-dir/x/y/z

  $ cat >sub-dir/dune <<EOF
  > (install
  >  (files a.txt (glob_files_rec x/*.txt))
  >  (section share))
  > EOF

  $ touch sub-dir/a.txt
  $ touch sub-dir/b.txt
  $ touch sub-dir/x/foo.txt
  $ touch sub-dir/x/y/foo.txt
  $ touch sub-dir/x/y/z/foo.txt

  $ dune build @sub-dir/all

  $ find _build/default/sub-dir | sort
  _build/default/sub-dir
  _build/default/sub-dir/a.txt
  _build/default/sub-dir/x
  _build/default/sub-dir/x/foo.txt
  _build/default/sub-dir/x/y
  _build/default/sub-dir/x/y/foo.txt
  _build/default/sub-dir/x/y/z
  _build/default/sub-dir/x/y/z/foo.txt
