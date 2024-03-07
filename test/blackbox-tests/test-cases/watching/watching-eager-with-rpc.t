Show error when trying to build through rpc when server is running eager watch

  $ echo '(lang dune 3.8)' > dune-project
  $ mkdir src
  $ echo '(library (name foo))' > src/dune
  $ touch src/a.ml
  $ touch src/b.ml

  $ dune build @all
  $ dune build --watch &
  Success, waiting for filesystem changes...

  $ dune rpc build --wait .
  Error: { payload = None
  ; message =
      "the rpc server is running with eager watch mode using --watch. to run builds through an rpc client, start the server using --passive-watch-mode"
  ; kind = Invalid_request
  }

  $ dune shutdown
  $ wait
