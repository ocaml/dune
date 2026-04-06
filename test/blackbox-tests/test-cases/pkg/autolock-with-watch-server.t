Test that auto-locking works correctly when dune is running in the watch mode.
Reproducer for https://github.com/ocaml/dune/issues/13234

  $ mkrepo
  $ add_mock_repo_if_needed
  $ enable_pkg
  $ mkpkg b 0.1
  $ mkpkg c 0.2

Make dune-project file with dependency on b:

  $ cat >dune-project <<EOF
  > (lang dune 3.22)
  > (package
  >  (name a)
  >  (depends b))
  > EOF

  $ cat > a.ml <<EOF
  > let () = print_endline "Hello world!"
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (public_name a))
  > EOF

Start dune (in passive watch mode)

  $ start_dune
  $ build a.exe
  Success
  $ cat .#dune-output
  Success, waiting for filesystem changes...

Add new dependency c:

  $ cat >dune-project <<EOF
  > (lang dune 3.22)
  > (package
  >  (allow_empty)
  >  (name a)
  >  (depends b c))
  > EOF

Run build:

  $ build a.exe
  Success

Stop the watch server

  $ stop_dune
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
