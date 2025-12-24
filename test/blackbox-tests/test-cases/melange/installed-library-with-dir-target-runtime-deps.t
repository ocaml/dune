Test `melange.runtime_deps` in a library that has been installed

  $ mkdir lib app prefix
  $ cat > lib/dune-project <<EOF
  > (lang dune 3.20)
  > (package (name foo))
  > (using directory-targets 0.1)
  > (using melange 1.0)
  > EOF

  $ echo 'hello' > lib/file.txt
  $ cat > lib/dune <<EOF
  > (rule (target (dir some_dir))
  >  (action
  >   (progn (system "mkdir %{target}")
  >    (system "echo hello from file inside dir target > %{target}/inside-dir-target.txt"))))
  > (library
  >  (public_name foo)
  >  (modes melange)
  >  (libraries melange.node)
  >  (preprocess (pps melange.ppx))
  >  (melange.runtime_deps ./some_dir ./file.txt))
  > EOF
  $ cat > lib/foo.ml <<EOF
  > let dirname = [%mel.raw "__dirname"]
  > let () = Js.log2 "dirname:" dirname
  > let file_path = "./index.txt"
  > let read_asset () = Node.Fs.readFileSync (dirname ^ "/" ^ file_path) \`utf8
  > EOF

  $ dune build
  $ dune install --prefix $PWD/prefix --display short
  Installing $TESTCASE_ROOT/prefix/lib/foo/META
  Installing $TESTCASE_ROOT/prefix/lib/foo/dune-package
  Installing $TESTCASE_ROOT/prefix/lib/foo/file.txt
  Installing $TESTCASE_ROOT/prefix/lib/foo/foo.ml
  Installing $TESTCASE_ROOT/prefix/lib/foo/melange/foo.cmi
  Installing $TESTCASE_ROOT/prefix/lib/foo/melange/foo.cmj
  Installing $TESTCASE_ROOT/prefix/lib/foo/melange/foo.cmt
  Installing $TESTCASE_ROOT/prefix/lib/foo/some_dir/inside-dir-target.txt

  $ rm -rf $PWD/prefix
  $ dune clean
  $ cat > lib/dune <<EOF
  > (rule (target (dir some_dir))
  >  (action
  >   (progn (system "mkdir %{target}")
  >    (system "echo hello from file inside dir target > %{target}/inside-dir-target.txt"))))
  > (library
  >  (public_name foo)
  >  (modes melange)
  >  (libraries melange.node)
  >  (preprocess (pps melange.ppx))
  >  (melange.runtime_deps ./some_dir/inside-dir-target.txt ./some_dir ./file.txt))
  > EOF

  $ dune build
  $ dune install --prefix $PWD/prefix --display short
  Installing $TESTCASE_ROOT/prefix/lib/foo/META
  Installing $TESTCASE_ROOT/prefix/lib/foo/dune-package
  Installing $TESTCASE_ROOT/prefix/lib/foo/file.txt
  Installing $TESTCASE_ROOT/prefix/lib/foo/foo.ml
  Installing $TESTCASE_ROOT/prefix/lib/foo/melange/foo.cmi
  Installing $TESTCASE_ROOT/prefix/lib/foo/melange/foo.cmj
  Installing $TESTCASE_ROOT/prefix/lib/foo/melange/foo.cmt
  Installing $TESTCASE_ROOT/prefix/lib/foo/some_dir/inside-dir-target.txt
