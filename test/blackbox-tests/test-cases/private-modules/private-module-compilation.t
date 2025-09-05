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

  $ cat > dune-project << EOF
  > (lang dune 3.0)
  > EOF

Build should fail because Secret is private:
  $ dune build
  File "consumer.ml", line 4, characters 17-40:
  4 |   print_endline (Mylib.Secret.get_hidden ())
                       ^^^^^^^^^^^^^^^^^^^^^^^
  Error: The module Mylib.Secret is an alias for module Mylib__Secret, which is missing
  [1]

Now test that removing private_modules makes it work:
  $ cat > mylib/dune << EOF
  > (library
  >  (name mylib))
  > EOF

  $ dune build
