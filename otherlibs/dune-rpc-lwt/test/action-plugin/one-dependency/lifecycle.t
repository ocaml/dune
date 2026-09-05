A request can outlive the plugin process. The dependency waits for its caller to
exit, so it cannot be recorded before the plugin finishes.

  $ cat > dune-project <<'EOF'
  > (lang dune 2.0)
  > (using action-plugin 0.1)
  > EOF
  $ cp bin/foo.exe .
  $ printf first > control
  $ cat > dune <<'EOF'
  > (rule
  >  (target slow-input)
  >  (deps control)
  >  (action
  >   (run sh -c "touch started; while kill -0 $(cat parent-pid 2>/dev/null) 2>/dev/null; do sleep 0.01; done; cat control > slow-input")))
  > (rule
  >  (target detached)
  >  (action (dynamic-run ./foo.exe detached)))
  > EOF
  $ timeout 5 dune build -j 2 detached
  ran

Accepted requests are drained before the rule is cached, even if their client
has disconnected. Their dependencies continue to invalidate the rule.

  $ printf second > control
  $ timeout 5 dune build -j 2 detached
  ran
  $ printf third > control
  $ timeout 5 dune build -j 2 detached
  ran
