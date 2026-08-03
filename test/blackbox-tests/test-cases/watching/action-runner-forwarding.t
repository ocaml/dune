A build command with action-runner flags cannot forward to an existing watch server.

  $ setup_xdg_runtime_dir
  $ make_dune_project 3.23
  $ cat > dune <<'EOF'
  > (rule
  >  (target x)
  >  (action (bash "echo ok > %{target}")))
  > EOF

  $ start_dune
  $ dune build --action-runner x
  Error: Action runner flags cannot be used when forwarding build requests to
  an existing Dune process. Start the server with the action runner flags
  instead.
  [1]
  $ dune build --sandbox-actions x
  Error: Action runner flags cannot be used when forwarding build requests to
  an existing Dune process. Start the server with the action runner flags
  instead.
  [1]
  $ shutdown_dune_quiet >/dev/null 2>&1
  $ wait_for_dune_exit_with_timeout >/dev/null 2>&1
  $ [ "$?" = 0 ]
