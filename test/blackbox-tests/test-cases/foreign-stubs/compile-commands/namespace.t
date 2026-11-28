The database's internal directory must not shadow explicitly included source
directories, whether or not the compilation database feature is available.

  $ for version in 3.22 3.23; do
  >   echo "$version"
  >   mkdir "v$version"
  >   (
  >     cd "v$version"
  >     make_dune_project "$version"
  >     echo '(dirs .compile_commands)' > dune
  >     mkdir .compile_commands
  >     echo '(rule (alias smoke) (action (echo ok)))' > .compile_commands/dune
  >     dune build @smoke && printf '\n'
  >   )
  > done
  3.22
  ok
  3.23
  ok
