This test demonstrate a dependency cycle if we consider nodes as modules, but a
valid dependency graph if we consider the implementations and interfaces of
modules as having separate dependencies.

Reproduces #7018

  $ cat >dune-project <<EOF
  > (lang dune 3.7)
  > EOF
  $ cat >dune <<EOF
  > (library
  >  (wrapped false)
  >  (name foobar))
  > EOF

  $ cat >x.mli <<EOF
  > type t = unit
  > EOF

  $ cat >y.mli <<EOF
  > val foo : X.t -> unit
  > EOF
  $ cat >y.ml <<EOF
  > let foo _ = ()
  > EOF

  $ runtest() {
  > cat >x.ml <<EOF
  > type t = unit
  > let () = Y.foo $1
  > EOF
  > dune build
  > }

First we try to construct X.t directly

  $ runtest "()"
  Error: dependency cycle between modules in _build/default:
     X
  -> X
  -> required by _build/default/foobar.a
  -> required by alias all
  -> required by alias default
  File "x.ml", line 2, characters 15-17:
  2 | let () = Y.foo ()
                     ^^
  Error: This expression has type t but an expression was expected of type X.t
         X.t is abstract because no corresponding cmi file was found in path.
  [1]

Now we use a polymorphic type:

  $ runtest "(assert false)" 
  Error: dependency cycle between modules in _build/default:
     X
  -> X
  -> required by _build/default/foobar.a
  -> required by alias all
  -> required by alias default
  [1]

Or, we can use another module:

  $ cat > unit.ml <<EOF
  > let x = ()
  > EOF
  $ cat > unit.mli <<EOF
  > val x : X.t
  > EOF

  $ runtest "Unit.x"
  Error: dependency cycle between modules in _build/default:
     X
  -> X
  -> required by _build/default/foobar.a
  -> required by alias all
  -> required by alias default
  [1]
