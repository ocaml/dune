X is not accessible as it is private
  $ dune build private-module/app/app__Foo.js
  File "app/foo.ml", line 1, characters 0-9:
  1 | Lib.X.run ();;
      ^^^^^^^^^
  Error: The module Lib.X is an alias for module Lib__X, which is missing
  [1]
