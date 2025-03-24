Test a case of virtual libraries where libraries depend on the virtual library
transitively

  $ mkdir -p foo vlib vlib_impl
  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (using melange 0.1)
  > EOF
  $ cat > foo/dune <<EOF
  > (library
  >  (name foo)
  >  (modes melange)
  >  (libraries vlib))
  > EOF
  $ cat > foo/virt.ml <<EOF
  > include Vlib.Virt
  > EOF

  $ cat > vlib/dune <<EOF
  > (library
  >  (name vlib)
  >  (modes melange)
  >  (virtual_modules virt))
  > EOF
  $ cat > vlib/virt.mli <<EOF
  > type t
  > EOF

  $ cat > vlib_impl/dune <<EOF
  > (library
  >  (name vlib_impl)
  >  (modes melange)
  >  (implements vlib))
  > EOF
  $ cat > vlib_impl/virt.ml <<EOF
  > type t = string ref
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (target js)
  >  (modules)
  >  (emit_stdlib false)
  >  (libraries foo vlib_impl))
  > EOF

  $ dune build @melange
