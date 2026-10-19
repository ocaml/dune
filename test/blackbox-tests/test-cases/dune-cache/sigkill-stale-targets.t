Killing dune with SIGKILL after an action wrote its targets but before
_build/.db is saved must not leave targets that the next build trusts and
stores in the shared cache.

See #16559.

  $ export DUNE_CACHE=enabled
  $ export DUNE_CACHE_ROOT=$PWD/cache

  $ make_dune_project 3.0
  $ cat > dune <<'EOF'
  > (rule (with-stdout-to x (cat %{dep:src})))
  > (rule (with-stdout-to y (progn (cat %{dep:x}) (cat %{dep:extra}))))
  > (rule (alias kill) (deps x) (action (system "kill -9 $PPID")))
  > EOF

  $ echo A > src
  $ echo 1 > extra
  $ dune build ./y

Rebuild x from a different source, then get killed before the database is
saved.

  $ echo B > src
  $ (dune build @kill; echo "exit: $?") 2> /dev/null
  exit: 137

Restore the original source. x must be rebuilt.

  $ echo A > src
  $ echo 2 > extra
  $ dune build ./y
  $ cat _build/default/x
  A
  $ rm -rf _build
  $ dune build ./y
  $ cat _build/default/y
  A
  2

A deleted target behaves the same way.

  $ echo B > src
  $ (dune build @kill; echo "exit: $?") 2> /dev/null
  exit: 137
  $ echo A > src
  $ rm _build/default/x
  $ dune build ./x
  $ test -e _build/default/x || echo missing
