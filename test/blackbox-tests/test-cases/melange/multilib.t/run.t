Compilation using melange with a library
The rules should include cmjs of dependencies as well.

  $ dune build x/.x.objs/melange/x__M_2.js

Make sure no byte folders are included.

  $ dune rules x/.x.objs/melange/x__M_2.js  | tr -s '\n' ' ' |
  > grep -ce "byte"
  0
  [1]

JavaScript files are generated.

  $ node ./_build/default/x/.x.objs/melange/x__M_2.js
  done
