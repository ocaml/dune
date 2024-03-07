Test for %{bin-available:...}

  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > (package (name foo))
  > EOF
  $ cat >dune<<"EOF"
  > (install
  >  (section bin)
  >  (enabled_if false)
  >  (files disabled))
  > (rule
  >  (alias available)
  >  (action
  >   (progn
  >    (echo "dune: %{bin-available:dune}\n")
  >    (echo "local program foo: %{bin-available:foo}\n")
  >    (echo "non existent program: %{bin-available:*}\n")
  >    (echo "local path foo: %{bin-available:./foo}\n")
  >    (echo "local path bar: %{bin-available:./bar}\n")
  >    (echo "disabled binary is available: %{bin-available:disabled}\n")
  >    (echo "disabled by enabled_if: %{bin-available:bar}\n"))))
  > 
  > (executable
  >  (public_name foo)
  >  (modules foo))
  > (executable
  >  (public_name bar)
  >  (modules bar)
  >  (enabled_if false))
  > EOF
  $ touch foo.ml bar.ml

  $ dune build @available
  dune: true
  local program foo: true
  non existent program: false
  local path foo: false
  local path bar: false
  disabled binary is available: false
  disabled by enabled_if: false
