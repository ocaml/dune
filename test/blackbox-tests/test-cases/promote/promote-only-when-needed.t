Test that targets aren't re-promoted if they are up to date.

  $ echo "(lang dune 3.0)" > dune-project
  $ cat >dune <<EOF
  > (rule
  >  (targets promoted)
  >  (mode promote)
  >  (action (with-stdout-to promoted (echo "Hello, world!"))))
  > EOF

  $ dune build promoted --verbose 2>&1 | grep "Promoting"
  Promoting "_build/default/promoted" to "promoted"
  $ cat promoted
  Hello, world!

Dune doesn't promote the file again if it's unchanged.

  $ dune build promoted --verbose 2>&1 | grep "Promoting"
  [1]
  $ cat promoted
  Hello, world!

Dune does promotes the file again if it's changed.

  $ echo hi > promoted
  $ dune build promoted --verbose 2>&1 | grep "Promoting"
  Promoting "_build/default/promoted" to "promoted"
  $ cat promoted
  Hello, world!

Now test behaviour for executables, which use artifact substitution.

  $ git init --quiet

  $ cat >dune <<EOF
  > (executable
  >  (name hello)
  >  (libraries dune-build-info)
  >  (promote))
  > EOF

  $ cat >hello.ml <<EOF
  > print_endline "Hello, World!";
  > (match Build_info.V1.version () with
  >   | Some _ -> print_endline "Has version info"
  >   | None -> print_endline "Has no version info")
  > EOF

  $ dune build hello.exe --verbose 2>&1 | grep "Promoting"
  Promoting "_build/default/hello.exe" to "hello.exe"
  $ ./hello.exe
  Hello, World!
  Has version info

Bug: Dune currently re-promotes versioned executables on every restart.

# CR-someday amokhov: Fix this.

  $ dune build hello.exe --verbose 2>&1 | grep "Promoting"
  Promoting "_build/default/hello.exe" to "hello.exe"
