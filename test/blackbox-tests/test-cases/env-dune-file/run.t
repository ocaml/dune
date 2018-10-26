env vars set in env should be visible to all subdirs
  $ dune build --root env-subdir
  Entering directory 'env-subdir'
  PASS

env vars intepreted in various fields, such as flags
  $ dune build --force --root flag-field
  Entering directory 'flag-field'
  var visible from dune: -principal         foo alias default
  DUNE_FOO: <not found>

global vars are overriden
  $ DUNE_FOO=blarg dune build --force --root flag-field
  Entering directory 'flag-field'
  var visible from dune: -principal         foo alias default
  DUNE_FOO: blarg
