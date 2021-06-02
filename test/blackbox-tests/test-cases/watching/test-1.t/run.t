  $ DUNE_RUNNING=0

  $ start_dune () {
  >  ((dune build "$@" -w --passive-watch-mode > dune-output 2>&1) || (echo $? >> dune-output)) &
  >   DUNE_PID=$!;
  >   DUNE_RUNNING=1;
  >   sleep 2
  > }

  $ with_timeout () {
  >   timeout 2 "$@"
  >   exit_code=$?
  >   if [ "$exit_code" = 124 ]
  >   then
  >     printf "Timed out"
  >   else
  >     return "$exit_code"
  >   fi
  > }


  $ build () {
  >   with_timeout dune rpc build "$@"
  > }

  $ start_dune

----------------------------------------------------------------------------------
* Compile a simple rule

  $ echo "(lang dune 2.0)" > dune-project

  $ cat > x <<EOF
  > original-contents
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (target y)
  >  (deps x)
  >  (action (bash "cat x > y")))
  > EOF

  $ echo wut
  wut

  $ build y
  Success
  $ cat _build/default/y
  original-contents

  $ echo new-contents > x

  $ build y
  Success
  $ cat _build/default/y
  new-contents

  $ echo new-contents2 > x

  $ build y
  Success
  $ cat _build/default/y
  new-contents2

  $ with_timeout dune rpc shutdown
  $ cat dune-output
  dune exited:
  Success, waiting for filesystem changes...
  
  ********** NEW BUILD **********
  
  Success, waiting for filesystem changes...
  
  ********** NEW BUILD **********
  
  Success, waiting for filesystem changes...
  0
