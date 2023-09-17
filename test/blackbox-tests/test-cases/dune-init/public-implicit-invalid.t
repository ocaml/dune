When a public name is implicit from the name, it should still be validated as a
public name.

  $ dune init lib 0 --public
  Error: Public names are composed of an opam package name and optional
  dot-separated string suffixes.
  Package names can contain letters, numbers, '-', '_' and '+', and need to
  contain at least a letter.
  [1]
