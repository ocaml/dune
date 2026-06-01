Test that virtual libraries with private modules work with public implementations
(Issue #10635)

  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (package (name pkg))
  > EOF

Virtual library with a private (non-virtual) helper module:

  $ make_private_module_virtual_lib_fixture

Implementation with public_name (but no private modules of its own):


Should build without "External.cm_dir" errors:

  $ dune build @install
