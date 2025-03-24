Test virtual libraries where the virtual lib is public and the concrete impl is
private

  $ mkdir -p vlib js_impl test
  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (using melange 0.1)
  > (package (name the_lib))
  > EOF
  $ cat > vlib/dune <<EOF
  > (library
  >  (name the_lib)
  >  (modes melange native)
  >  (public_name the_lib)
  >  (virtual_modules virt))
  > EOF
  $ cat > vlib/the_lib.mli <<EOF
  > module Time : sig
  >   val gettimeofday : unit -> float
  > end
  > EOF
  $ cat > vlib/the_lib.ml <<EOF
  > module Time = struct
  >   let gettimeofday () = Virt.gettimeofday ()
  > end
  > EOF
  $ cat > vlib/virt.mli <<EOF
  > val gettimeofday : unit -> float
  > EOF

  $ cat > js_impl/dune <<EOF
  > (library
  >  (name timeJs)
  >  (implements the_lib)
  >  (modes melange)
  >  (preprocess (pps melange.ppx)))
  > EOF
  $ cat > js_impl/virt.ml <<EOF
  > let gettimeofday : unit -> float = fun () -> 42.
  > EOF

  $ cat > test/dune <<EOF
  > (melange.emit
  >  (target output)
  >  (libraries the_lib timeJs)
  >  (emit_stdlib false))
  > EOF

  $ dune build @melange
  File "js_impl/dune", lines 1-5, characters 0-95:
  1 | (library
  2 |  (name timeJs)
  3 |  (implements the_lib)
  4 |  (modes melange)
  5 |  (preprocess (pps melange.ppx)))
  Error: Dune doesn't currently support building private implementations of
  virtual public libaries for `(modes melange)`
  Hint: Add a `public_name` to the library `timeJs'.
  [1]

Making timeJs a public library makes it work

  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (using melange 0.1)
  > (package (name the_lib))
  > (package (name concrete_lib))
  > EOF

  $ cat > js_impl/dune <<EOF
  > (library
  >  (name timeJs)
  >  (public_name concrete_lib)
  >  (implements the_lib)
  >  (modes melange)
  >  (preprocess (pps melange.ppx)))
  > EOF

  $ dune build @melange
