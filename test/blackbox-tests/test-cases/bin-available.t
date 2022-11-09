Test for %{bin-available:...}

  $ cat >dune-project <<"EOF"
  > (lang dune 3.0)
  > (package (name foo))
  > EOF
  $ cat >dune<<"EOF"
  > (rule
  >  (alias default)
  >  (action
  >   (progn
  >    (echo "dune: %{bin-available:dune}\n")
  >    (echo "local program foo: %{bin-available:foo}\n")
  >    (echo "non existent program: %{bin-available:*}\n"))))
  > 
  > (executable (public_name foo))
  > EOF
  $ touch foo.ml

  $ dune build
  dune: true
  local program foo: true
  non existent program: false
