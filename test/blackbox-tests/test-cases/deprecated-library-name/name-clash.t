The old_public_name field is named in a confusing matter. As we can see in this
test, the name may refer to private names in the current scope as well as public
names.

  $ cat >dune-project <<EOF
  > (lang dune 2.0)
  > (package (name a))
  > (package (name b))
  > EOF

  $ cat >dune <<EOF
  > (deprecated_library_name (old_public_name b) (new_public_name a))
  > (library (name b) (public_name a.b))
  > EOF

  $ dune build @all
  Error: Library b is defined twice:
  - dune:2
  - dune:1
  [1]
