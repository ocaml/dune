Test the case where a test that depends on a melange emit alias

  $ cat <<EOF > dune-project
  > (lang dune 3.8)
  > (using melange 0.1)
  > EOF

  $ mkdir test

  $ cat > dune <<EOF
  > (melange.emit
  >  (target app)
  >  (alias app-alias))
  > EOF

  $ cat > a.ml <<EOF
  > let () = Js.log "foo"
  > EOF

  $ cat > test/dune <<EOF
  > (cram
  >  (deps (alias_rec %{workspace_root}/app-alias)))
  > EOF

The expected behavior would be that all .js artifacts are copied over to the
test folder, but only the empty folders structured is copied.

  $ cat > test/foo.t <<EOF
  >   $ ls ..
  >   app
  >   test
  >   $ ls ../app
  >   node_modules
  > EOF

  $ dune build @foo

Adding (expand_aliases_in_sandbox) does not affect it

  $ cat <<EOF > dune-project
  > (lang dune 3.8)
  > (using melange 0.1)
  > (expand_aliases_in_sandbox)
  > EOF

  $ dune build @foo

