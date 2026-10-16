Dune starts an RPC server on every build. When the socket cannot be created,
for instance in a sandbox forbidding sockets, the build should still succeed.

We make socket creation fail by making the socket directory read-only.

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > EOF

  $ cat > dune <<EOF
  > (rule (with-stdout-to x (echo "hello")))
  > EOF

  $ mkdir -p _build/.rpc
  $ chmod -w _build/.rpc

By default, the build proceeds without the RPC server.

  $ dune build ./x
  $ cat _build/default/x
  hello

Passive watch mode cannot work without the RPC server, so failing to start it
is a hard error.

  $ dune build --passive-watch-mode 2> stderr
  [1]

Neutralize the error message, which differs in a sandbox that forbids `socket`
altogether.

  $ sed -E 's/^Error: (bind|socket)\(\): .*/Error: <socket failure>/' stderr
  Error: <socket failure>
