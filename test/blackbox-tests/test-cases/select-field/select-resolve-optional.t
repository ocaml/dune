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

