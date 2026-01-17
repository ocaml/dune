Test that virtual libraries with private modules work with public implementations
(Issue #10635)

  $ mkdir -p vlib impl
  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > (using melange 0.1)
  > (package (name pkg))
  > EOF

Virtual library with a private (non-virtual) helper module:

  $ cat > vlib/dune <<EOF
  > (library
  >  (name vlib)
  >  (modes byte melange)
  >  (public_name pkg.vlib)
  >  (virtual_modules virt)
  >  (private_modules helper))
  > EOF
  $ cat > vlib/vlib.ml <<EOF
  > let run () = Virt.run () + Helper.value
  > EOF
  $ cat > vlib/vlib.mli <<EOF
  > val run : unit -> int
  > EOF
  $ cat > vlib/virt.mli <<EOF
  > val run : unit -> int
  > EOF
  $ cat > vlib/helper.ml <<EOF
  > let value = 42
  > EOF

Implementation with public_name (but no private modules of its own):

  $ cat > impl/dune <<EOF
  > (library
  >  (name impl)
  >  (modes byte melange)
  >  (public_name pkg.impl)
  >  (implements pkg.vlib))
  > EOF
  $ cat > impl/virt.ml <<EOF
  > let run () = 1
  > EOF

Should build without "External.cm_dir" errors:

  $ dune build @install
  Error: Multiple rules generated for
  _build/install/default/lib/pkg/impl/.private/vlib__Helper.cmi:
  - impl/dune:1
  - impl/dune:1
  -> required by _build/default/pkg.install
  -> required by alias install
  Error: Multiple rules generated for
  _build/install/default/lib/pkg/vlib/.private/vlib__Helper.cmt:
  - vlib/dune:1
  - vlib/dune:1
  -> required by _build/default/pkg.install
  -> required by alias install
  [1]
