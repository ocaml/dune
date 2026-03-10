Test demonstrating the handling of actions that produce symlinks.

  $ echo "(lang dune 2.8)" > dune-project
  $ cat >dune <<EOF
  > (rule (targets b) (deps a) (action (run ln -s a b)))
  > EOF
  $ echo a > a
  $ dune build ./b

  $ function print_trace() {
  > dune trace cat | jq '
  > include "dune";
  >   processes
  > | .args
  > | .prog |= basename
  > | select(.prog == "ln")
  > | {prog,process_args}
  > '
  > }

  $ print_trace
  {
    "prog": "ln",
    "process_args": [
      "-s",
      "a",
      "b"
    ]
  }

  $ readlink _build/default/b
  a
  $ cat _build/default/b
  a

  $ dune build ./b

  $ print_trace

  $ echo a-v2 > a
  $ dune build ./b

  $ print_trace
  {
    "prog": "ln",
    "process_args": [
      "-s",
      "a",
      "b"
    ]
  }

  $ cat _build/default/b
  a-v2

Demonstrate the current behaviour where promoting the target turns the symlink
into a regular file.

# CR-someday amokhov: We should probably fix this.

  $ cat >dune <<EOF
  > (rule (mode promote) (targets b) (deps a) (action (run ln -s a b)))
  > EOF

  $ dune build ./b
  $ cat b
  a-v2
  $ dune_cmd stat kind _build/default/b
  symbolic link
  $ dune_cmd stat kind b
  regular file
