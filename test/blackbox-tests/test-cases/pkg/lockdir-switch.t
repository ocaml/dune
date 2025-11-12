This test checks what happens when you lock with default directory then change
workspace to use a custom lock directory.

  $ . ./helpers.sh
  $ mkrepo
  $ mkpkg foo
  $ add_mock_repo_if_needed

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > (package
  >  (name myproject)
  >  (depends foo))
  > EOF

Lock with default directory name:

  $ dune pkg lock
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - foo.0.0.1

Now change the workspace to use a custom lock directory:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.21)
  > (lock_dir
  >  (path custom.lock)
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (url "file://$PWD/mock-opam-repository"))
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (public_name bar))
  > EOF

  $ cat > bar.ml <<EOF
  > let () = print_endline "hello from bar" ;;
  > EOF

  $ dune exec -- bar
  Error: No rule found for default/.lock/dune.lock (context _private)
  -> required by lock directory environment for context "default"
  -> required by base environment for context "default"
  -> required by loading findlib for context "default"
  -> required by creating installed environment for "default"
  [1]

Now try with a context stanza that references the custom lock directory with a
context stanza:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.21)
  > (context
  >  (default
  >   (lock_dir custom.lock)))
  > (lock_dir
  >  (path custom.lock)
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (url "file://$PWD/mock-opam-repository"))
  > EOF

This is working as intended. The error is not relevant here.

  $ dune exec -- bar 
  hello from bar
