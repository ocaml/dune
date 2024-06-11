Test if the issue #274 is solved: the tyxml.functor is included with -H flag in
ITD = false case, and thus no type abstraction happens when it is used.

  $ ocamlc -H x --help > /dev/null
  $ if [ $? = 0 ]; then dune build fi
