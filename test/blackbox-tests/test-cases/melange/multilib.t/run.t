Compilation using melange with a library
The rules should include cmjs of dependencies as well.

Make sure no byte folders are included.

  $ dune rules multilib/x/m_2.js  | tr -s '\n' ' ' |
  > grep -ce "byte"
  0
  [1]

Test resulting file

  $ dune build multilib/c.js
  $ node ./_build/default/multilib/c.js
  done
