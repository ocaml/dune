Verify that library file deps are declared for module compilation rules.

Every non-alias module should declare glob deps on its library
dependencies' .cmi files.

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > EOF

  $ mkdir lib
  $ cat > lib/dune <<EOF
  > (library (name mylib))
  > EOF
  $ cat > lib/mylib.ml <<EOF
  > let value = 42
  > EOF
  $ cat > lib/mylib.mli <<EOF
  > val value : int
  > EOF

  $ cat > dune <<EOF
  > (executable (name main) (libraries mylib))
  > EOF
  $ cat > uses_lib.ml <<EOF
  > let get () = Mylib.value
  > EOF
  $ cat > uses_lib.mli <<EOF
  > val get : unit -> int
  > EOF
  $ cat > main.ml <<EOF
  > let () = print_int (Uses_lib.get ())
  > EOF

  $ dune build ./main.exe

Both modules declare glob deps on mylib's .cmi files:

  $ dune rules --deps _build/default/.main.eobjs/native/dune__exe__Uses_lib.cmx 2>&1 | grep 'predicate'
     (predicate *.cmi)

  $ dune rules --deps _build/default/.main.eobjs/native/dune__exe__Main.cmx 2>&1 | grep 'predicate'
     (predicate *.cmi)
