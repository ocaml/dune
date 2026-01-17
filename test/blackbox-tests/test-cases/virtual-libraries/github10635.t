Test that virtual libraries with private modules work with public implementations
(Issue #10635)

  $ mkdir -p vlib impl
  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (package (name pkg))
  > EOF

Virtual library with a private (non-virtual) helper module:

  $ cat > vlib/dune <<EOF
  > (library
  >  (name vlib)
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
  >  (public_name pkg.impl)
  >  (implements pkg.vlib))
  > EOF
  $ cat > impl/virt.ml <<EOF
  > let run () = 1
  > EOF

Should build without "External.cm_dir" errors:

  $ dune build @install 2>&1 | grep -i "code_error\|external.cm_dir" || true
    ("External.cm_dir",
  Raised at Stdune__Code_error.raise in file
    "otherlibs/stdune/src/code_error.ml", line 10, characters 30-62
