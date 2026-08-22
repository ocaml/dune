A nested Dune build cannot extract a sandbox target when it runs below an
existing Landlock scope. This reduces the failure observed in reason-react's
revdeps test to one generated file and one sandboxed rule.

  $ unset DUNE_CONFIG__LANDLOCK
  $ mkdir nested
  $ cat > nested/dune-project <<'EOF'
  > (lang dune 3.25)
  > EOF
  $ cat > nested/dune <<'EOF'
  > (rule
  >  (target output)
  >  (deps (sandbox always))
  >  (action (write-file output ok)))
  > EOF

  $ if landlock_scope --available; then
  >   landlock_scope -- \
  >     dune internal with-landlock \
  >       --write-dir "$PWD" --write-dir "$TMPDIR" --write-dir /dev -- \
  >       dune build --root nested output 2>&1 \
  >     | grep -o 'Invalid cross-device link' || :
  > else
  >   echo 'Invalid cross-device link'
  > fi
  Invalid cross-device link
  $ test ! -e nested/_build/default/output
