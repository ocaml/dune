Documentation of parameterised libraries, their parameters, implementations and
instantiations.

  $ init_oxcaml_project
  $ cat >> dune-project <<EOF
  > (package (name project))
  > EOF

A parameter:

  $ make_dir_with_dune "param" <<EOF
  > (library_parameter
  >  (public_name project.param)
  >  (name param))
  > EOF
  $ cat > param/param.mli <<EOF
  > (** The parameter's documentation. *)
  > val v : string
  > EOF

An implementation of that parameter:

  $ make_dir_with_dune "param_impl" <<EOF
  > (library
  >  (public_name project.param_impl)
  >  (name param_impl)
  >  (implements param))
  > EOF
  $ cat > param_impl/param_impl.ml <<EOF
  > (** The implementation's documentation. *)
  > let v = "impl"
  > EOF

A parameterised library and its instantiation:

  $ make_dir_with_dune "plib" <<EOF
  > (library
  >  (public_name project.plib)
  >  (name plib)
  >  (parameters param))
  > EOF
  $ cat > plib/plib.ml <<EOF
  > (** The parameterised library's documentation. *)
  > let v = Param.v
  > EOF

  $ make_dir_with_dune "user" <<EOF
  > (library
  >  (public_name project.user)
  >  (name user)
  >  (libraries (instantiate plib param_impl)))
  > EOF
  $ cat > user/user.ml <<EOF
  > (** The user's documentation. *)
  > let v = Plib.v
  > EOF

A virtual library and its implementation, whose documentation is carried by the
virtual library itself:

  $ make_dir_with_dune "virt" <<EOF
  > (library
  >  (public_name project.virt)
  >  (name virt)
  >  (virtual_modules virt))
  > EOF
  $ cat > virt/virt.mli <<EOF
  > (** The virtual library's documentation. *)
  > val v : string
  > EOF

  $ make_dir_with_dune "virt_impl" <<EOF
  > (library
  >  (public_name project.virt_impl)
  >  (name virt_impl)
  >  (implements virt))
  > EOF
  $ cat > virt_impl/virt.ml <<EOF
  > let v = "virt"
  > EOF

Every library gets its documentation, except the implementation of the virtual
library:

  $ dune build @doc
  $ find _build/default/_doc/_odocls -name '*.odocl' | sort
  _build/default/_doc/_odocls/project/page-index.odocl
  _build/default/_doc/_odocls/project/param.odocl
  _build/default/_doc/_odocls/project/param_impl.odocl
  _build/default/_doc/_odocls/project/plib.odocl
  _build/default/_doc/_odocls/project/user.odocl
  _build/default/_doc/_odocls/project/virt.odocl

The package index lists them all:

  $ cat _build/default/_doc/_mlds/project/index.mld
  {0 project index}
  {1 Library project.param}
  The entry point of this library is the module:
  {!module-Param}.
  {1 Library project.param_impl}
  The entry point of this library is the module:
  {!module-Param_impl}.
  {1 Library project.plib}
  The entry point of this library is the module:
  {!module-Plib}.
  {1 Library project.user}
  The entry point of this library is the module:
  {!module-User}.
  {1 Library project.virt}
  The entry point of this library is the module:
  {!module-Virt}.

The new documentation generator agrees:

  $ dune build @doc-new 2>/dev/null
  $ find _build/default/_doc_new/odoc/local -name '*.odocl' | sort
  _build/default/_doc_new/odoc/local/project/param/param.odocl
  _build/default/_doc_new/odoc/local/project/param_impl/param_impl.odocl
  _build/default/_doc_new/odoc/local/project/plib/plib.odocl
  _build/default/_doc_new/odoc/local/project/user/user.odocl
  _build/default/_doc_new/odoc/local/project/virt/virt.odocl
