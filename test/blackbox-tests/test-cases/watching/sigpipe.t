We demonstrate that dune can be turned off by sigpipe

  $ echo '(lang dune 3.0)' > dune-project
  $ touch dout
  $ dune build --passive-watch-mode > dout 2>&1 &
  $ pid=$!
  $ kill -s PIPE $pid
  $ wait $pid
  [141]
  $ cat dout
