Testing that the instantiation of parameterised preprocessed modules work,
since modules have a different filename ending in `.pp.ml` after the
preprocessing.

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > (using oxcaml 0.1)
  > EOF

First define a parameter:

  $ mkdir param
  $ cat > param/param.mli <<EOF
  > val param : string
  > EOF
  $ cat > param/dune <<EOF
  > (library_parameter
  >  (name param)
  >  (preprocess
  >   (action
  >    (progn
  >     (cat %{input-file})
  >     (echo "val pp : string")))))
  > EOF

Then an implementation:

  $ mkdir impl
  $ cat > impl/impl.ml <<EOF
  > let param = "impl"
  > EOF
  $ cat > impl/dune <<EOF
  > (library
  >  (name impl)
  >  (implements param)
  >  (preprocess
  >   (action
  >    (progn
  >     (cat %{input-file})
  >     (echo "let pp = \"%{input-file}\"")))))
  > EOF

Then a parameterised library, with at least two files to trigger the generation
of `.pp.ml`:

  $ mkdir lib
  $ cat > lib/lib.ml <<EOF
  > let lib () = Util.util () ^ ", " ^ Util.pp
  > EOF
  $ cat > lib/util.ml <<EOF
  > let util () = Param.param ^ ", " ^ Param.pp
  > EOF
  $ cat > lib/dune <<EOF
  > (library
  >  (name lib)
  >  (parameters param)
  >  (preprocess
  >   (action
  >    (progn
  >     (cat %{input-file})
  >     (echo "let pp = \"%{input-file}\"")))))
  > EOF

Then an executable:

  $ mkdir bin
  $ cat > bin/bin.ml <<EOF
  > let () = print_endline (Lib.lib () ^ ", " ^ Lib.pp)
  > EOF
  $ cat > bin/dune <<EOF
  > (executable
  >  (name bin)
  >  (libraries (instantiate lib impl)))
  > EOF

Finally run it:

  $ dune exec ./bin/bin.exe
  impl, impl/impl.ml, lib/util.ml, lib/lib.ml
