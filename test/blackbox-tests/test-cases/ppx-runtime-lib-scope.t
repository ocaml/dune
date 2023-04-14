Create ppx1, ppx2 and exe:
- ppx1 is a regular ppx rewriter
- ppx2 is also a ppx rewriter, preprocessed with ppx1
- exe depends on ppx2

  $ mkdir -p project/ppx/ppx1 project/ppx/ppx2 project/exe
  $ cat > project/ppx/dune-project <<EOF
  > (lang dune 3.7)
  > (package (name the-ppx))
  > EOF
  $ cat > project/ppx/ppx1/dune <<EOF
  > (library
  >  (name ppx1)
  >  (public_name the-ppx.ppx1)
  >  (kind ppx_rewriter)
  >  (ppx.driver (main Ppx1.main)))
  > EOF
  $ cat > project/ppx/ppx1/ppx1.ml <<EOF
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
  $ cat > project/ppx/ppx2/dune <<EOF
  > (library
  >  (name ppx2)
  >  (public_name the-ppx.ppx2)
  >  (preprocess (pps ppx1)) ; changing to the-ppx.ppx1 makes it work
  >  (kind ppx_rewriter))
  > EOF
  $ cat > project/ppx/ppx2/ppx2.ml <<EOF
  > let main () = ()
  > EOF

  $ cat > project/dune-project <<EOF
  > (lang dune 3.7)
  > (package (name the-exe))
  > EOF
  $ cat > project/exe/dune <<EOF
  > (executable
  >  (name the_exe)
  >  (public_name the-exe)
  >  (libraries the-ppx.ppx2))
  > EOF
  $ touch project/exe/the_exe.ml

  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build --root project --display short ./exe/the_exe.exe
  Entering directory 'project'
  File "ppx/ppx2/dune", line 4, characters 18-22:
  4 |  (preprocess (pps ppx1)) ; changing to the-ppx.ppx1 makes it work
                        ^^^^
  Error: Library "ppx1" not found.
  -> required by _build/default/ppx/ppx2/ppx2.pp.ml
  -> required by _build/default/ppx/ppx2/.ppx2.objs/byte/ppx2.cmi
  -> required by _build/default/exe/.the_exe.eobjs/byte/dune__exe__The_exe.cmi
  -> required by
     _build/default/exe/.the_exe.eobjs/native/dune__exe__The_exe.cmx
  -> required by _build/default/exe/the_exe.exe
  Leaving directory 'project'
  [1]
