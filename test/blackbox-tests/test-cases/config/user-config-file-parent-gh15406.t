A regular file where the user config directory should be must not make Dune
crash while probing the default user config file.

  $ echo '(lang dune 3.0)' > dune-project
  $ touch dune

  $ mkdir .config
  $ touch .config/dune
  $ (
  >   unset INSIDE_DUNE
  >   output=dune.out
  >   XDG_CONFIG_HOME=$(dune_cmd native-path "$PWD/.config") dune build > "$output" 2>&1
  >   status=$?
  >   if [ $status -ne 0 ]; then
  >     if grep -q 'Unix.Unix_error(Unix.ENOTDIR' "$output"; then
  >       echo "dune crashed with ENOTDIR"
  >     else
  >       cat "$output"
  >     fi
  >     exit $status
  >   fi
  >   cat "$output"
  > )
  dune crashed with ENOTDIR
  [2]
