Testing that vendoring a parameterised library works as expected:

  $ make_dune_project 3.20

Note that this root `dune-project` does not enables `(using oxcaml 0.1)`, but
will depend on a vendored project that does.

We first set up a simple binary that depends on a vendored library:

  $ cat > dune <<EOF
  > (executable (name bin) (libraries vendored.vendored_lib))
  > (vendored_dirs vendored)
  > EOF

  $ cat > bin.ml <<EOF
  > let () = print_endline (Vendored_lib.vendored_lib ())
  > EOF

Then the vendored project:

  $ mkdir vendored
  $ cat > vendored/dune-project <<EOF
  > (lang dune 3.20)
  > (using oxcaml 0.1)
  > (package (name vendored))
  > EOF

The parameter definition must reside in a separate folder from its implementation:

  $ mkdir vendored/param
  $ cat > vendored/param/dune <<EOF
  > (library_parameter (public_name vendored.param) (name param) (modules param))
  > EOF

  $ write_vendored_dune() {
  >   local param_impl_public_name="${1:-}"
  >   local lib_param_public_name="${2:-}"
  >   local vendored_lib_public_name="${3:-}"
  > 
  >   {
  >     echo "(library"
  >     if [ -n "$param_impl_public_name" ]; then
  >       echo "   (public_name ${param_impl_public_name})"
  >     fi
  >     echo "   (name param_impl)"
  >     echo "   (modules param_impl)"
  >     echo "   (implements param))"
  >     echo ""
  >     echo "(library"
  >     if [ -n "$lib_param_public_name" ]; then
  >       echo "  (public_name ${lib_param_public_name})"
  >     fi
  >     echo "  (name lib_param)"
  >     echo "  (modules lib_param)"
  >     echo "  (parameters param))"
  >     echo ""
  >     echo "(library"
  >     if [ -n "$vendored_lib_public_name" ]; then
  >       echo "  (public_name ${vendored_lib_public_name})"
  >     fi
  >     echo "  (name vendored_lib)"
  >     echo "  (modules vendored_lib)"
  >     echo "  (libraries"
  >     echo "    (instantiate lib_param param_impl)))"
  >     echo ""
  >     echo "(executable"
  >     echo "  (name vendored_bin)"
  >     echo "  (modules vendored_bin)"
  >     echo "  (libraries vendored_lib))"
  >   } > vendored/dune
  > }

Then three vendored libraries, one for the implementation of the parameter, one
parameterised library, one library which depends on its instantation, and a
non-public executable:

  $ write_vendored_dune vendored.param_impl vendored.lib_param vendored.vendored_lib

A simple implementation for each:

  $ echo 'val v : string' > vendored/param/param.mli
  $ echo 'let v = "impl"' > vendored/param_impl.ml
  $ echo 'let lib_param () = "lib_param:" ^ Param.v' > vendored/lib_param.ml
  $ echo 'let vendored_lib () = "vendored:" ^ Lib_param.lib_param ()' > vendored/vendored_lib.ml
  $ echo 'let () = print_endline ("vendored_bin:" ^ Vendored_lib.vendored_lib ())' > vendored/vendored_bin.ml

Running the executable requires instantiating the vendored
`(lib_param param_impl)` dependency:

  $ dune exec ./bin.exe
  vendored:lib_param:impl
  $ ls _build/default/.parameterised/*/vendored.lib_param
  vendored.lib_param!vendored.param_impl

Similarly for the vendored binary:

  $ dune exec ./vendored/vendored_bin.exe
  vendored_bin:vendored:lib_param:impl

This run uses the same instantiation as the root, so no `vendored`-specific
instantation are created:

  $ ls vendored/_build/default/.parameterised/*
  ls: cannot access 'vendored/_build/default/.parameterised/*': No such file or directory
  [2]

But building the executable from the vendored directory doesn't:

  $ dune exec --root=vendored ./vendored_bin.exe
  vendored_bin:vendored:lib_param:impl
  $ ls vendored/_build/default/.parameterised/*/vendored.lib_param
  vendored.lib_param!vendored.param_impl

Since the vendored binary only depends on public libraries, we get a single
scope:

  $ ls _build/default/.parameterised/ | censor
  $DIGEST

But the vendored binary could use private libraries:

  $ write_vendored_dune

  $ dune clean
  $ dune exec ./vendored/vendored_bin.exe
  vendored_bin:vendored:lib_param:impl

Which requires a new scope to host the private instantiation of `(lib_param
param_impl)`:

  $ ls _build/default/.parameterised/ | censor
  $DIGEST
  $ ls _build/default/.parameterised/*
  lib_param
  $ ls _build/default/.parameterised/*/lib_param
  lib_param!param_impl

We can also have a mix of public/private, with e.g. the parameterised library
being public:

  $ write_vendored_dune "" vendored.lib_param

  $ dune clean
  $ dune exec ./vendored/vendored_bin.exe
  vendored_bin:vendored:lib_param:impl
  $ ls -R _build/default/.parameterised | censor
  _build/default/.parameterised:
  $DIGEST
  
  _build/default/.parameterised/$DIGEST:
  vendored.lib_param
  
  _build/default/.parameterised/$DIGEST/vendored.lib_param:
  vendored.lib_param!param_impl
  
  _build/default/.parameterised/$DIGEST/vendored.lib_param/vendored.lib_param!param_impl:
  archive.a
  archive.cmxa

Or only the parameter being public:

  $ write_vendored_dune vendored.param_impl

  $ dune clean
  $ dune exec ./vendored/vendored_bin.exe
  vendored_bin:vendored:lib_param:impl
  $ tree _build/default/.parameterised | censor
  _build/default/.parameterised
  `-- $DIGEST
      `-- lib_param
          `-- lib_param!vendored.param_impl
              |-- archive.a
              `-- archive.cmxa
  
  4 directories, 2 files

The parameter itself could be private too,

  $ cat > vendored/param/dune <<EOF
  > (library_parameter (name param) (modules param))
  > EOF

  $ dune clean
  $ dune exec ./vendored/vendored_bin.exe
  File "vendored/dune", line 5, characters 15-20:
  5 |    (implements param))
                     ^^^^^
  Error: Library "param" is private, it cannot be a dependency of a public
  library. You need to give "param" a public name.
  [1]

But only if there are no public stanza depending on it:

  $ write_vendored_dune

  $ dune exec ./vendored/vendored_bin.exe
  vendored_bin:vendored:lib_param:impl
