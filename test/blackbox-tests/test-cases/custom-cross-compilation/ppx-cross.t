Demonstrate a bad interaction between cross-compilation and
ppx_runtime_libraries

  $ mkdir -p etc/findlib.conf.d
  $ export OCAMLFIND_CONF=$PWD/etc/findlib.conf
  $ touch etc/findlib.conf etc/findlib.conf.d/foo.conf

Create lib1, ppx and lib2:
- lib1 is a regular library
- ppx has a runtime dep on lib1 (via `ppx_runtime_libraries`)
- lib2 is a regular library pre-processed by `ppx`

  $ mkdir lib1 ppx lib2
  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (package (name ppx-cross))
  > EOF

  $ cat > lib1/dune <<EOF
  > (library
  >  (name lib1)
  >  (public_name ppx-cross.lib1))
  > EOF

In the ppx, removing `ppx_runtime_libraries` makes the test pass

  $ cat > ppx/dune <<EOF
  > (library
  >  (name ppx)
  >  (public_name ppx-cross.ppx)
  >  (kind ppx_rewriter)
  >  (ppx.driver (main Ppx.main))
  >  (ppx_runtime_libraries lib1))
  > EOF
  $ cat > ppx/ppx.ml <<EOF
  > let main () =
  >   let out = ref "" in
  >   let args =
  >     [ ("-o", Arg.Set_string out, "")
  >     ; ("--impl", Arg.Set_string (ref ""), "")
  >     ; ("--as-ppx", Arg.Set (ref false), "")
  >     ; ("--cookie", Arg.Set (ref false), "")
  >     ]
  >   in
  >   let anon _ = () in
  >   Arg.parse (Arg.align args) anon "";
  >   let out = open_out !out in
  >   close_out out;
  > EOF

  $ cat > lib2/dune <<EOF
  > (library
  >  (name lib2)
  >  (public_name ppx-cross.lib2)
  >  (preprocess (pps ppx)))
  > EOF
  $ touch lib2/lib2.ml

  $ dune build @install -x foo
  Error: Conflict between the following libraries:
  - "ppx-cross.lib1" in _build/default.foo/lib1
  - "ppx-cross.lib1" in _build/default/lib1
  -> required by _build/default.foo/lib2/.lib2.objs/byte/lib2.cmt
  -> required by _build/install/default.foo/lib/ppx-cross/lib2/lib2.cmt
  -> required by _build/default.foo/ppx-cross-foo.install
  -> required by alias install (context default.foo)
  [1]

