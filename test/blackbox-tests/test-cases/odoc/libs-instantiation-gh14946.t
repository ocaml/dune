Generating the rules of the project root must not evaluate the `enabled_if`
of the libraries of the packages. Otherwise, a library that reads a file
generated in the root (an ancestor of the library's directory) makes the root
depend on itself.

  $ make_dune_project_with_package 3.15 foo

  $ echo true > config.in
  $ cat > dune << EOF
  > (rule (copy config.in config))
  > EOF

  $ mkdir lib
  $ cat > lib/dune << EOF
  > (library
  >  (name foo)
  >  (public_name foo)
  >  (enabled_if (= %{read:../config} true)))
  > EOF
  $ touch lib/foo.ml

Building a target of the root only loads the rules of the root:

  $ dune build ./config
