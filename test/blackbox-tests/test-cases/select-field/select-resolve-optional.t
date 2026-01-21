Show that failing to resolve an optional library via `(select ..)` falls back
to the default select branch.

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > EOF

  $ mkdir -p mylib_opt
  $ cat > mylib_opt/dune <<EOF
  > (library
  >  (name mylib_opt)
  >  (libraries external_lib_not_found))
  > EOF

  $ cat > mylib_opt/mylib_opt.ml <<EOF
  > let greeting = "Hello from mylib_opt"
  > EOF

  $ mkdir -p mylib_base
  $ cat > mylib_base/dune <<EOF
  > (library
  >  (name mylib_base)
  >  (libraries
  >   (select mylib_base.ml from
  >    (mylib_opt -> mylib_base.with.ml)
  >    (-> mylib_base.without.ml))))
  > EOF

  $ cat > mylib_base/mylib_base.with.ml <<EOF
  > let msg = Mylib_opt.greeting
  > EOF

  $ cat > mylib_base/mylib_base.without.ml <<EOF
  > let msg = "No mylib_opt"
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name main)
  >  (libraries mylib_base))
  > EOF

  $ cat > main.ml <<EOF
  > let () = print_endline Mylib_base.msg
  > EOF

  $ dune build main.exe
  File "mylib_opt/dune", line 3, characters 12-34:
  3 |  (libraries external_lib_not_found))
                  ^^^^^^^^^^^^^^^^^^^^^^
  Error: Library "external_lib_not_found" not found.
  -> required by library "mylib_opt" in _build/default/mylib_opt
  -> required by executable main in dune:2
  -> required by _build/default/.main.eobjs/native/dune__exe__Main.cmx
  -> required by _build/default/main.exe
  [1]

