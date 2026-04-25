A [.mli] change in a dependency library rebuilds consumers under
both the release (opaque=false) and dev (opaque=true) profiles.
Opaque mode does not affect [.cmi] propagation; it only affects
whether cross-module inlining tracks a dep's [.cmx].

Companion to [opaque.t], which covers the [.ml]-only change axis.

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF

  $ mkdir lib
  $ cat > lib/dune <<EOF
  > (library (name mylib))
  > EOF
  $ cat > lib/mylib.ml <<EOF
  > let v = 42
  > EOF
  $ cat > lib/mylib.mli <<EOF
  > val v : int
  > EOF

  $ cat > dune <<EOF
  > (executable (name main) (libraries mylib))
  > EOF
  $ cat > main.ml <<EOF
  > let () = print_int Mylib.v
  > EOF

--- Release profile (opaque=false): .mli change rebuilds consumer ---

  $ cat > dune-workspace <<EOF
  > (lang dune 3.0)
  > (profile release)
  > EOF

  $ dune build ./main.exe

Add a new declaration to both [.ml] and [.mli] (paired so no value is
left unexported, which would trip warning 32 under dev):

  $ cat > lib/mylib.ml <<EOF
  > let v = 42
  > let extra () = 0
  > EOF
  $ cat > lib/mylib.mli <<EOF
  > val v : int
  > val extra : unit -> int
  > EOF

  $ dune build ./main.exe
  $ dune trace cat | jq -s 'include "dune"; ([.[] | targetsMatchingFilter(test("dune__exe__Main"))] | length) > 0'
  true

--- Dev profile (opaque=true): .mli change still rebuilds consumer ---

  $ cat > dune-workspace <<EOF
  > (lang dune 3.0)
  > (profile dev)
  > EOF

  $ dune build ./main.exe

Add another paired declaration:

  $ cat > lib/mylib.ml <<EOF
  > let v = 42
  > let extra () = 0
  > let helper x = x + 1
  > EOF
  $ cat > lib/mylib.mli <<EOF
  > val v : int
  > val extra : unit -> int
  > val helper : int -> int
  > EOF

  $ dune build ./main.exe
  $ dune trace cat | jq -s 'include "dune"; ([.[] | targetsMatchingFilter(test("dune__exe__Main"))] | length) > 0'
  true
