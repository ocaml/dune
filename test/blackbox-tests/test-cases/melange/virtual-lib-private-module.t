Test that virtual libraries with private modules work with public implementations
(Issue #10635)

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > (using melange 0.1)
  > (package (name pkg))
  > EOF

Virtual library with a private (non-virtual) helper module:

  $ make_private_module_virtual_lib_fixture '(modes byte melange)'

Implementation with public_name (but no private modules of its own):


Should build without "External.cm_dir" errors:

  $ dune build @install

  $ dune install --display=short --prefix=out 2>&1 | grep '\.private'
  Installing out/lib/pkg/impl/.private/vlib__Helper.cmi
  Installing out/lib/pkg/impl/melange/.private/vlib__Helper.cmi
  Installing out/lib/pkg/vlib/.private/vlib__Helper.cmi
  Installing out/lib/pkg/vlib/.private/vlib__Helper.cmt
  Installing out/lib/pkg/vlib/melange/.private/vlib__Helper.cmi
  Installing out/lib/pkg/vlib/melange/.private/vlib__Helper.cmt

