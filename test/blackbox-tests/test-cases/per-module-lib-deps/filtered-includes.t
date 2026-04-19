When per-module dependency filtering restricts which libraries a module
depends on, the -I flags should also be restricted to only include
directories for those libraries. This prevents accidental compilation
success when a module references a library it doesn't declare, and
improves caching by not invalidating unrelated modules when a new
dependency is added.

See https://github.com/ocaml/dune/pull/14116#discussion_r3077939968

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > EOF

Create two libraries:

  $ mkdir mylib
  $ cat > mylib/dune <<EOF
  > (library (name mylib))
  > EOF
  $ cat > mylib/mylib_util.ml <<EOF
  > let x = 42
  > EOF

  $ mkdir otherlib
  $ cat > otherlib/dune <<EOF
  > (library (name otherlib))
  > EOF
  $ cat > otherlib/otherlib_util.ml <<EOF
  > let y = 99
  > EOF

Create an executable that depends on both libraries, with two modules:
one that references mylib and one that references nothing external.

  $ cat > dune <<EOF
  > (executable
  >  (name main)
  >  (libraries mylib otherlib))
  > EOF

  $ cat > main.ml <<EOF
  > let () = print_int (Uses_mylib.get ())
  > EOF

  $ cat > uses_mylib.ml <<EOF
  > let get () = Mylib.Mylib_util.x
  > EOF

  $ cat > no_deps.ml <<EOF
  > let msg = "I don't use any library"
  > EOF

  $ dune build ./main.exe

Helper to extract -I directories from compilation rules (excluding the
module's own obj dir):

  $ extract_includes () {
  >   dune rules "$1" 2>&1 | tr -s '() \n' '\n' | \
  >     awk '/^-I$/{getline; print}' | grep -v '\.main\.' | sort -u || true
  > }

The module uses_mylib references Mylib, so its -I flags should include
mylib but not otherlib:

  $ extract_includes _build/default/.main.eobjs/byte/dune__exe__Uses_mylib.cmo
  mylib/.mylib.objs/byte

The module no_deps references no external library, so its -I flags
should not include either library:

  $ extract_includes _build/default/.main.eobjs/byte/dune__exe__No_deps.cmo
