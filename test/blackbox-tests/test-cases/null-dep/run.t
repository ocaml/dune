  $ dune runtest --debug-dependency-path
  File "dune", line 3, characters 25-32:
  3 |  (action (with-stdout-to %{null} (echo "hello world"))))
                               ^^^^^^^
  Error: target /dev/null is outside the build directory. This is not allowed.
  [1]
