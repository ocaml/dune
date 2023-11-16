%{lib:.....} forms should expand in the correct context.
See #9199.

  $ cat > dune-project << EOF
  > (lang dune 1.1)
  > 
  > (package
  >  (name somelib))
  > EOF

  $ cat > dune-workspace << EOF
  > (lang dune 1.10)
  > (context
  >  (default
  >   (name host)))
  > (context
  >  (default
  >   (name cross)
  >   (host host)))
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (write-file target %{lib:somelib:somelib.a}))
  > EOF

  $ mkdir lib
  $ cat > lib/dune << EOF
  > (library
  >  (public_name somelib))
  > EOF

  $ dune build _build/host/target _build/cross/target
  $ cat _build/host/target
  ../install/host/lib/somelib/somelib.a
  $ cat _build/cross/target
  ../install/cross/lib/somelib/somelib.a
