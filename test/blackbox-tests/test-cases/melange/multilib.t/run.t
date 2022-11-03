Compilation using melange with a library
The rules should include cmjs of dependencies as well.

Make sure no byte folders are included.

  $ dune rules multilib/x/x__M_2.js  | tr -s '\n' ' ' |
  > grep -ce "byte"
  0
  [1]

Test resulting file

  $ dune build multilib/melange__C.js
  $ node ./_build/default/multilib/melange__C.js
  done
