Test a case of virtual libraries where the virtual library is transitive

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
  Error: No rule found for test/output/node_modules/the_lib/the_lib.js
  -> required by alias test/melange
  Error: No rule found for test/output/node_modules/the_lib/the_lib__.js
  -> required by alias test/melange
  [1]
