If we have a 'pure' system dependency (no OCaml compilation unit)
and no 'libraries' field, we don't fail nor do we print a hint

  $ mkrepo
  $ add_mock_repo_if_needed

  $ cat > dune-project << EOF
  > (lang dune 3.20)
  > (package
  >  (name foo)
  >  (depends conf-foobar))
  > EOF

  $ mkdir src

  $ cat > src/dune << EOF
  > (executable
  >  (name foo)
  >  (public_name foo))
  > EOF

  $ cat > src/foo.ml << EOF
  > let _exit_code = Sys.command "foobar some_arg" in
  > ()
  > EOF

  $ mkpkg conf-foobar << EOF
  > build: [
  >   ["foobar" "--version"]
  > ]
  > depexts: [
  >   ["foobar-cram"] 
  > ]
  > flags: conf
  > EOF

Dune is aware of the dependency
  $ dune show depexts
  foobar-cram

And we add it to the lock solution
  $ make_lockdir
  $ make_lockpkg conf-foobar << EOF
  > (version 0.0.1)
  > 
  > (build
  >  (run foobar --version))
  > 
  > (depexts foobar-cram)
  > EOF

Yet we don't try to build it at all
  $ dune build

And we don't print the hint
  $ dune exec src/foo.exe 2>&1 | sed 's/.*foobar.*not found/foobar not found/'
  foobar not found

If however, the 'libraries' field is present and there is an unknown 'hi'
lib required, dune will try building depexts to find said 'hi' module,
even if it is unrelated to our depext.

  $ rm src/dune
  $ cat > src/dune << EOF
  > (executable
  >  (name foo)
  >  (public_name foo)
  >  (libraries hi))
  > EOF

Now we see it!
  $ dune build
  File "dune.lock/conf-foobar.pkg", line 4, characters 6-12:
  4 |  (run foobar --version))
            ^^^^^^
  Error: Program foobar not found in the tree or in PATH
   (context: default)
  Hint: You may want to verify the following depexts are installed:
  - foobar-cram
  [1]
