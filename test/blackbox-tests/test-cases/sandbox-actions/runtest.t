`dune runtest --sandbox-actions` routes test actions through the worker too.

  $ make_dune_project 3.23
  $ readlink /proc/self/ns/mnt > host-ns
  $ cat > dune <<'EOF'
  > (rule
  >  (alias runtest)
  >  (deps host-ns)
  >  (action
  >   (bash "test \"$(readlink /proc/self/ns/mnt)\" != \"$(cat %{dep:host-ns})\" && echo runtest-inside")))
  > EOF

  $ dune runtest --sandbox-actions
  runtest-inside
