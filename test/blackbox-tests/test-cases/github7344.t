In #7344 there is a bug with dune coq top not finding the correct project file.
In infact, this bug appears to be prevelent in dune build also which we will
reproduce the case for here:

  $ cat > dune-project << EOF
  > (lang dune 3.8)
  > EOF
  $ mkdir src
  $ cat > src/dune << EOF
  > (library
  >  (name test))
  > EOF

As expected the regular `dune build` works:

  $ dune build

But the one in the subdirectory fails to find the root of the project:

  $ cd src
  $ dune build
  Warning: No dune-project file has been found. A default one is assumed but
  the project might break when dune is upgraded. Please create a dune-project
  file.
  Hint: generate the project file with: $ dune init project <name>
