Menhir inference must also avoid shadowed aliases before OCaml 4.08 (#8989).
Report an older compiler version to exercise this path on current compilers.

  $ real_ocamlc=$(command -v ocamlc)
  $ mkdir compiler
  $ cat >compiler/ocamlc <<EOF
  > #!/bin/sh
  > case "\$1" in
  >   -config) "$real_ocamlc" "\$@" | sed 's/^version: .*/version: 4.07.1/' ;;
  >   *) exec "$real_ocamlc" "\$@" ;;
  > esac
  > EOF
  $ chmod +x compiler/ocamlc
  $ ln -s "$(command -v ocamldep)" compiler/ocamldep
  $ export PATH="$PWD/compiler:$PATH"

Use nested groups to exercise multiple alias opens. The parser header must
still allow first-class module unpacking and generative functor applications.

  $ make_menhir_project 3.25 3.0
  $ cat >dune <<'EOF'
  > (include_subdirs qualified)
  > (library
  >  (name lib)
  >  (flags (:standard -w +63 -warn-error +63)))
  > EOF
  $ mkdir -p outer/inner
  $ echo 'module Inner = Inner' >outer/outer.ml
  $ echo 'module type S = sig end' >outer/ast.ml
  $ echo 'type t = T let x = T' >outer/inner/m.ml
  $ echo 'type t val x : t' >outer/inner/m.mli
  $ cat >outer/inner/dune <<'EOF'
  > (menhir
  >  (modules inner))
  > EOF
  $ cat >outer/inner/inner.mly <<'EOF'
  > %{
  > module M = M
  > module Unpacked = (val (module struct end : Ast.S) : Ast.S)
  > module Make () : sig type t val x : t end = struct
  >   type t = int
  >   let x = 1
  > end
  > module Fresh = Make ()
  > %}
  > %token EOF
  > %start <_> main
  > %%
  > main: EOF { (M.x, (module struct end : Ast.S)) }
  > EOF

  $ dune build lib.cma

The query opens the private alias interface through an anonymous functor
argument. The unit functor permits unpacking and generative applications.

  $ sed -n '1,/^module M = M$/p' _build/default/outer/inner/inner__mock.ml.mock
  include (functor
    (Dune__menhir__edcd073bac44eed37a6c3e073dd9ef1d : module type of struct
      include Dune__menhir__edcd073bac44eed37a6c3e073dd9ef1d
    end) ->
    functor () -> struct
    open! Dune__menhir__edcd073bac44eed37a6c3e073dd9ef1d
  
  type token = 
    | EOF
  
  # 1 "outer/inner/inner.mly"
    
  module M = M
  $ tail -n 2 _build/default/outer/inner/inner__mock.ml.mock
  
  end) (struct include Dune__menhir__edcd073bac44eed37a6c3e073dd9ef1d end) ()

The private interface includes the enclosing aliases in their original order.

  $ cat _build/default/.lib.objs/dune__menhir__*.mli
  include module type of struct
    include Lib
    include Lib__Outer__
    include Lib__Outer__Inner__
  end

The inferred interface refers to the original modules, not the functor argument.

  $ cat _build/default/outer/inner/inner__mock.mli.inferred
  type token = EOF
  module M = Lib__Outer__Inner__M
  module Unpacked : Lib__Outer__Ast.S
  module Make : () -> sig type t val x : t end
  module Fresh : sig type t val x : t end
  val menhir_begin_marker : int
  val xv_main : M.t * (module Lib__Outer__Ast.S)
  val menhir_end_marker : int
