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
  File "test/dune", line 5, characters 12-19:
  5 |  (libraries mel-foo))
                  ^^^^^^^
  Error: Library "mel-foo" not found.
  [1]
