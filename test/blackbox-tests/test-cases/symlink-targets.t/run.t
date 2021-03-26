Test demonstrating the handling of actions that produce symlinks.

  $ echo "(lang dune 2.8)" > dune-project
  $ cat >dune <<EOF
  > (rule (targets b) (deps a) (action (bash "ln -s a b")))
  > EOF
  $ echo a > a
  $ dune build ./b --display=short
          bash b
  $ readlink _build/default/b
  a
  $ cat _build/default/b
  a

  $ dune build ./b --display=short


  $ echo a-v2 > a
  $ dune build ./b --display=short
          bash b
  $ cat _build/default/b
  a-v2
