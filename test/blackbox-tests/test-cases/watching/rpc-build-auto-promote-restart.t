RPC build requests wait for rebuilds caused by auto-promotion before completing.

The first build of the alias below fails the diff action and auto-promotes the
correction. The source change invalidates the just-finished build, so the RPC
request must not complete with the initial failure; it should wait for the
watch loop to rebuild the alias successfully.

  $ make_dune_project 3.23
  $ cat > source <<EOF
  > old
  > EOF
  $ cat > dune <<EOF
  > (rule
  >  (target source.corrected)
  >  (action (write-file %{target} "new\n")))
  > (rule
  >  (alias repro)
  >  (action (diff source source.corrected)))
  > EOF

  $ start_dune --auto-promote
  $ with_timeout dune rpc build --wait '(alias repro)'
  Success
  $ cat source
  new
  $ stop_dune_quiet
