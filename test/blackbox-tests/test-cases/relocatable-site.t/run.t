  $ dune build @install
  $ mkdir install
  $ dune install --prefix /tmp/install --relocatable 2> /dev/null
Expect the placeholder to be replaced by 'share/test',
not by '/tmp/install/share/test'.
  $ grep /tmp/install/bin/test -e "=10:share/test"
  Binary file /tmp/install/bin/test matches
