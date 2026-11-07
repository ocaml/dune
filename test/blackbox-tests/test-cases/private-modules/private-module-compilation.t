Test demonstrating private modules in wrapped library

  $ mkdir mylib
  $ cat > mylib/dune << EOF
  > (library
  >  (name mylib)
  >  (private_modules secret))
  > EOF

  $ cat > mylib/secret.ml << EOF
  > let hidden_value = "This should be private"
  > let get_hidden () = hidden_value
  > EOF

  $ cat > mylib/public.ml << EOF
  > let exposed_value = "This is public"
  > let access_secret () = Secret.get_hidden ()
  > EOF

  $ cat > consumer.ml << EOF
  > let () =
  >   print_endline Mylib.Public.exposed_value;
  >   (* Try to access private module through the library's main module *)
  >   print_endline (Mylib.Secret.get_hidden ())
  > EOF

  $ cat > dune << EOF
  > (executable
  >  (name consumer)
  >  (libraries mylib))
  > EOF

  $ make_dune_project 3.0

Without sandboxing, private modules are reachable through the wrapper:

  $ dune build --sandbox none

Now test that removing private_modules makes it work:
  $ cat > mylib/dune << EOF
  > (library
  >  (name mylib))
  > EOF

  $ dune build

With sandboxing, resolving the wrapper alias also needs the hidden CMI directory.

  $ if ocamlc -H . -version > /dev/null 2>&1; then
  >   cat > mylib/dune <<'EOF'
  > (library (name mylib) (private_modules secret))
  > EOF
  >   cat > dune <<'EOF'
  > (executable (name consumer) (modes byte) (libraries mylib))
  > EOF
  >   dune build --sandbox none --build-dir _build-none consumer.bc &&
  >   ! dune build --sandbox copy --cache=disabled --build-dir _build-copy \
  >     consumer.bc > errors 2>&1 &&
  >   grep -q 'alias for module Mylib__Secret, which is missing$' errors &&
  >   echo 'let _ = Mylib__Secret.hidden_value' > consumer.ml &&
  >   ! dune build --sandbox copy --cache=disabled --build-dir _build-copy \
  >     consumer.bc > errors 2>&1 &&
  >   grep -q '^Error: Unbound module Mylib__Secret$' errors
  > fi
