rules with targets outside the build dir are dot allowed
  $ dune build @all
  File "dune", line 1, characters 6-53:
  1 | (rule (with-stdout-to /abs/path (system "echo toto")))
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: target /abs/path is outside the build directory. This is not allowed.
  [1]
