Test what happens when trying to build aliases over rpc

  $ . ./helpers.sh

  $ echo '(lang dune 3.16)' > dune-project
  $ cat > dune <<EOF
  > (rule
  >  (targets y)
  >  (deps x)
  >  (action (system "cat x > y")))
  > (alias
  >  (name foo)
  >  (deps y))
  > EOF

  $ echo 1 > x

When called normally, it can build the alias ok

  $ dune build @foo

Trying to build the alias through rpc fails

  $ start_dune

  $ build @foo
  Failure

  $ stop_dune
  Error: Don't know how to build @foo
  Had 1 error, waiting for filesystem changes...
