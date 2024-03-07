Test that melange.emit targets are not included in @install for packages they
don't belong to.

  $ mkdir lib test ppx
  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > (package (name my-ppx))
  > (package (name mel-foo))
  > (using melange 0.1)
  > EOF

  $ cat > ppx/dune <<EOF
  > (library
  >  (name my_ppx)
  >  (public_name my-ppx))
  > EOF
  $ touch ppx/my_ppx.ml

  $ cat > lib/dune <<EOF
  > (library
  >  (public_name mel-foo)
  >  (name mel_foo)
  >  (modes melange)
  >  (preprocess (pps melange.ppx)))
  > EOF
  $ cat > lib/mel_foo.ml <<EOF
  > let x = "lib"
  > EOF

  $ cat > test/dune <<EOF
  > (melange.emit
  >  (package mel-foo)
  >  (target js-out)
  >  (emit_stdlib false)
  >  (libraries mel-foo))
  > EOF
  $ cat > test/test_entry.ml <<EOF
  > let () = Js.log Mel_foo.x
  > EOF

`melange.emit` is attached to the package `mel-foo`, so it shouldn't be built
when building the other library

  $ dune build -p my-ppx

It still builds everything normally with the alias

  $ dune build @melange
  $ ls _build/default/test/js-out/test/
  test_entry.js

Now define a 3rd lib that we'll use to preprocess the melange.emit entries:

  $ dune clean
  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > (package (name my-ppx))
  > (package (name mel-foo))
  > (package (name my-ppx2))
  > (using melange 0.1)
  > EOF
  $ mkdir ppx2
  $ cat > ppx2/dune <<EOF
  > (library
  >  (name my_ppx2)
  >  (kind ppx_rewriter) (libraries ppxlib)
  >  (public_name my-ppx2))
  > EOF
  $ touch ppx2/my_ppx2.ml
  $ cat > test/dune <<EOF
  > (melange.emit
  >  (package mel-foo)
  >  (target js-out)
  >  (preprocess (pps my-ppx2))
  >  (emit_stdlib false)
  >  (libraries mel-foo))
  > EOF

we can still build my-ppx independently

  $ dune build -p my-ppx

and fails if it can't resolve libraries to build the alias

  $ dune build @melange -p my-ppx
  File "test/dune", line 6, characters 12-19:
  6 |  (libraries mel-foo))
                  ^^^^^^^
  Error: Library "mel-foo" not found.
  -> required by _build/default/test/js-out/test/test_entry.js
  -> required by alias test/melange
  [1]

