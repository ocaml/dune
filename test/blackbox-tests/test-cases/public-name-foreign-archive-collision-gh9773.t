When a library has a public name that collides with the name of a foreign archive, a "multiple rules generated" error happens.
Particularly, if the foreign archive is in another subdirectory, the loc for the foreign archive will point at the buildable definition.

See #9773.

  $ cat > dune-project << EOF
  > (lang dune 3.0)
  > (package
  >  (name libsomething))
  > EOF

  $ cat > dune << EOF
  > (library
  >  (public_name libsomething)
  >  (foreign_archives native/something))
  > EOF

  $ mkdir native
  $ cat > native/dune << EOF
  > (rule
  >  (target dllsomething.so)
  >  (action
  >   (run %{cc} -shared -o %{target} %{dep:src.o})))
  > 
  > (rule
  >  (target libsomething.a)
  >  (action
  >   (run ar rcs %{target} %{dep:src.o})))
  > 
  > (rule
  >  (target src.o)
  >  (action
  >   (run %{cc} -c %{dep:src.c})))
  > EOF

  $ cat > native/src.c << EOF
  > int data = 50;
  > EOF

  $ dune build
  Error: Multiple rules generated for
  _build/install/default/lib/libsomething/libsomething.a:
  - dune:1
  - dune:1
  -> required by _build/default/libsomething.install
  -> required by alias all
  -> required by alias default
  [1]
