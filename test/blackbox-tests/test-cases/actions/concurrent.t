Specification of the concurrency action:

  $ cat > dune-project << EOF
  > (lang dune 3.7)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (action
  >   (concurrent )))
  > EOF

  $ dune build
  File "dune", line 3, characters 2-15:
  3 |   (concurrent )))
        ^^^^^^^^^^^^^
  Error: 'concurrent' is only available since version 3.8 of the dune language.
  Please update your dune-project file to have (lang dune 3.8).
  [1]

Requires Dune 3.8.

  $ cat > dune-project << EOF
  > (lang dune 3.8)
  > EOF

(concurrent ...) runs actions concurrently. Here we mock up an example where two
subactions rely on eachother to also be running in order to terminate.

We write a shell script that will simultaneously read and write to two named
pipes. (This has to be similtaneious otherwise the read will block the write).
They will block on the read however, which means if called with the same two
pipes but swapped, they will only terminate when both scripts are running at the
same time.
  $ cat > run.sh << EOF
  > echo foo>\$1 & read line<\$2
  > EOF

We create an action that will create named pipes a and b and then run our script
on both of them, but importantly inside the concurrent action. This will
demonstrate that subactions are indeed being run concurrently. 
  $ cat > dune << EOF
  > (rule
  >  (deps run.sh)
  >  (alias my-rule)
  >  (action
  >   (progn
  >    (run mkfifo a b)
  >    (concurrent
  >     (run sh run.sh a b)
  >     (run sh run.sh b a)))))
  > EOF

When we run the rule, we see that the two actions are indeed run concurrently.
  $ dune build -j2 @my-rule --force

Notice the need for a -j2. If Dune was configured with -j1 then the action would
never terminate.
