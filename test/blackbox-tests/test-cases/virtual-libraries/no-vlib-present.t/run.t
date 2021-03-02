Test that implementing vlibs that aren't present is impossible
  $ dune build
  File "dune", line 3, characters 13-27:
  3 |  (implements foobar12312414))
                   ^^^^^^^^^^^^^^
  Error: Library "foobar12312414" not found.
  [1]
