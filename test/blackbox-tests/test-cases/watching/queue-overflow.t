Regression test for https://github.com/ocaml/dune/issues/16052.

A file-watcher queue overflow drops every Memo table. Nodes retained through
dependencies must remain connected to entries subsequently looked up in those
tables. Simulate the overflow through a private RPC rather than relying on an
operating system to exhaust its event queue.

  $ setup_xdg_runtime_dir
  $ cat > dune-project <<'EOF'
  > (lang dune 3.0)
  > EOF
  $ cat > dune <<'EOF'
  > (rule
  >  (alias default)
  >  (deps
  >   (glob_files *.in))
  >  (target all.out)
  >  (action
  >   (with-stdout-to
  >    all.out
  >    (echo "%{deps}"))))
  > EOF

  $ : > first.in
  $ start_dune
  $ build .
  Success
  $ cat _build/default/all.out
  ./first.in

The RPC returns after the build triggered by the simulated overflow finishes.

  $ with_timeout dune rpc simulate-file-watcher-queue-overflow --wait

A later event must invalidate the node retained by the source tree so that a
forwarded build observes the new input.

  $ : > second.in
  $ with_timeout dune rpc flush-file-watcher --wait
  $ build .
  Success
  $ cat _build/default/all.out
  ./first.in ./second.in

  $ stop_dune_quiet
