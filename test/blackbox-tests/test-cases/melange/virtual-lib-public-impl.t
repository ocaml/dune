Test virtual libraries where the virtual implementation is a public library

  $ mkdir -p vlib js_impl test
  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (using melange 0.1)
  > (package (name the_lib))
  > (package (name concrete_lib))
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
  >  (public_name concrete_lib)
  >  (implements the_lib)
  >  (modes melange))
  > EOF
  $ cat > js_impl/virt.ml <<EOF
  > let gettimeofday : unit -> float = fun () -> 42.
  > EOF

  $ cat > test/dune <<EOF
  > (melange.emit
  >  (target output)
  >  (libraries the_lib concrete_lib)
  >  (emit_stdlib false))
  > EOF

  $ dune build @melange
