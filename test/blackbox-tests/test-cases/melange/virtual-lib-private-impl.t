Test virtual libraries where the virtual lib is public and the concrete impl is
private

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
  >  ;(public_name concrete_lib)
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

  $ ls _build/default/test/output
  js_impl
  node_modules
  test

  $ cat _build/default/test/output/node_modules/the_lib/the_lib.js
  // Generated by Melange
  'use strict';
  
  const Curry = require("melange.js/curry.js");
  const The_lib__Virt = require("js_impl/virt.js");
  
  function gettimeofday(param) {
    return Curry._1(The_lib__Virt.gettimeofday, undefined);
  }
  
  const Time = {
    gettimeofday: gettimeofday
  };
  
  exports.Time = Time;
  /* The_lib__Virt Not a pure module */

