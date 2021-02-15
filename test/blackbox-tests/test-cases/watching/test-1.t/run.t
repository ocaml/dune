  $ DUNE_RUNNING=0

  $ wait_for_build () {
  >   if [ "$DUNE_RUNNING" = 0 ]; then printf "dune already exited";
  >   else
  >     read x < pipe-from-dune;
  >     if [ "$x" = "exiting" ]; then echo "dune exited:"; cat dune-output; DUNE_RUNNING=0; fi
  >   fi
  > }

  $ start_building () {
  >  ((dune build "$@" -w --watch-on-build-finished './on-build-finished' > dune-output 2>&1 && (echo 0 >> dune-output; echo exiting > pipe-from-dune)) || (echo $? >> dune-output; echo exiting > pipe-from-dune & read x < pipe-to-dune)) &
  >   DUNE_PID=$!;
  >   DUNE_RUNNING=1
  >   wait_for_build
  > }

  $ stop_building () {
  >   if [ "$DUNE_RUNNING" = 0 ]; then printf "dune already exited"; fi
  >   echo 0 > pipe-to-dune;
  >   wait_for_build
  >   wait "$DUNE_PID";
  > }; true

  $ continue_building () {
  >   if [ "$DUNE_RUNNING" = 0 ]; then printf "dune already exited"; return 0; fi
  >   echo 1 > pipe-to-dune
  >   wait_for_build
  > }; true

  $ mkfifo pipe-to-dune
  $ mkfifo pipe-from-dune

  $ cat > on-build-finished <<'EOF'
  > #!/usr/bin/env bash
  > echo 'x' > pipe-from-dune
  > read x < pipe-to-dune
  > [ "$x" = 1 ]
  > EOF

  $ chmod +x on-build-finished

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

  $ start_building y
  $ cat _build/default/y
  original-contents

  $ echo new-contents > x

  $ continue_building
  $ cat _build/default/y
  new-contents

  $ echo new-contents2 > x

  $ continue_building
  $ cat _build/default/y
  new-contents2

  $ stop_building
  dune exited:
  
  ********** NEW BUILD **********
  
  
  ********** NEW BUILD **********
  
  0
