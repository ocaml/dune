Test that dune fails fast when the build lock is held but no RPC server is
available. This is a regression test for
https://github.com/ocaml/dune/issues/12900

  $ cat > dune-project <<EOF
  > (lang dune 3.18)
  > EOF

  $ cat > dune <<EOF
  > (executable (name foo))
  > EOF

  $ cat > foo.ml <<EOF
  > let () = print_endline "hello"
  > EOF

Helper to run a command while holding the build lock without an RPC server:

  $ with_build_lock_held() {
  >   mkdir -p _build
  >   (
  >     flock -x 9
  >     printf '1' > _build/.lock
  >     "$@"
  >   ) 9>_build/.lock
  > }

Hold the lock without running an RPC server, then try various commands.
They should all fail immediately instead of hanging.

  $ with_build_lock_held dune build
  Error: Another dune instance (pid: 1) has the build directory locked but is
  not running an RPC server.
  [1]

  $ with_build_lock_held dune exec ./foo.exe
  Error: Another dune instance (pid: 1) has the build directory locked but is
  not running an RPC server.
  [1]

  $ with_build_lock_held dune fmt
  Error: Another dune instance (pid: 1) has the build directory locked but is
  not running an RPC server.
  [1]

  $ with_build_lock_held dune promote
  Error: Another dune instance (pid: 1) has the build directory locked but is
  not running an RPC server.
  [1]

  $ with_build_lock_held dune runtest
  Error: Another dune instance (pid: 1) has the build directory locked but is
  not running an RPC server.
  [1]

We are not starting an RPC server for clean, but we do care if the lock is
held.

  $ with_build_lock_held dune clean
  Error: A running dune (pid: 1) instance has locked the build directory. If
  this is not the case, please delete "_build/.lock".
  [1]

Explicit RPC commands do not check the lock first, so they get a simpler error:

  $ dune rpc ping
  Error: RPC server not running.
  [1]

  $ dune rpc build
  Error: RPC server not running.
  [1]

  $ dune shutdown
  Error: RPC server not running.
  [1]

  $ dune diagnostics
  Error: RPC server not running.
  [1]

Commands that start their own RPC server will fail when trying to acquire the
lock:

  $ with_build_lock_held dune utop . 2>&1 | head -3
  Error: A running dune (pid: 1) instance has locked the build directory. If
  this is not the case, please delete "_build/.lock".

  $ with_build_lock_held dune ocaml top 2>&1 | head -3
  Error: A running dune (pid: 1) instance has locked the build directory. If
  this is not the case, please delete "_build/.lock".

  $ with_build_lock_held dune printenv 2>&1 | head -3
  Error: A running dune (pid: 1) instance has locked the build directory. If
  this is not the case, please delete "_build/.lock".

  $ with_build_lock_held dune describe workspace 2>&1 | head -3
  Error: A running dune (pid: 1) instance has locked the build directory. If
  this is not the case, please delete "_build/.lock".

