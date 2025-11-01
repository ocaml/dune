Testing that vendoring a parameterized library works as expected:

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > EOF

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

Then three vendored libraries, one for the implementation of the parameter, one
parameterised library, one library which depends on its instantation, and a
non-public executable:

  $ cat > vendored/dune <<EOF
  > (library
  >    (public_name vendored.param_impl)
  >    (name param_impl)
  >    (modules param_impl)
  >    (implements param))
  > 
  > (library 
  >   (public_name vendored.lib_param) 
  >   (name lib_param) 
  >   (modules lib_param) 
  >   (parameters param))
  > 
  > (library 
  >   (public_name vendored.vendored_lib) 
  >   (name vendored_lib) 
  >   (modules vendored_lib) 
  >   (libraries 
  >   (lib_param param_impl)))
  > 
  > (executable 
  >   (name vendored_bin) 
  >   (modules vendored_bin) 
  >   (libraries vendored_lib))
  > EOF

A simple implementation for each:

  $ echo 'val v : string' > vendored/param/param.mli
  $ echo 'let v = "impl"' > vendored/param_impl.ml
  $ echo 'let lib_param () = "lib_param:" ^ Param.v' > vendored/lib_param.ml
  $ echo 'let vendored_lib () = "vendored:" ^ Lib_param.lib_param ()' > vendored/vendored_lib.ml
  $ echo 'let () = print_endline ("vendored_bin:" ^ Vendored_lib.vendored_lib ())' > vendored/vendored_bin.ml

Running the executable requires instantiating the vendored
`(lib_param param_impl)` depdendency:

  $ dune exec ./bin.exe
  vendored:lib_param:impl
  $ ls _build/default/.parameterised/vendored.lib_param
  vendored.lib_param!vendored.param_impl

Similarly for the vendored binary:

  $ dune exec ./vendored/vendored_bin.exe
  vendored_bin:vendored:lib_param:impl

This run uses the same instantiation as the root, so no `vendored`-specific
instantation are created:

  $ ls vendored/_build/default/.parameterised/vendored.lib_param
  ls: cannot access 'vendored/_build/default/.parameterised/vendored.lib_param': No such file or directory
  [2]

But building the executable from the vendored directory doesn't:

  $ dune exec --root=vendored ./vendored_bin.exe
  Entering directory 'vendored'
  Leaving directory 'vendored'
  vendored_bin:vendored:lib_param:impl
  $ ls vendored/_build/default/.parameterised/vendored.lib_param
  vendored.lib_param!vendored.param_impl

Same if built from the vendored directory:

  $ cd vendored
  $ dune clean
  $ dune exec ./vendored_bin.exe
  vendored_bin:vendored:lib_param:impl
  $ ls _build/default/.parameterised/vendored.lib_param
  vendored.lib_param!vendored.param_impl
