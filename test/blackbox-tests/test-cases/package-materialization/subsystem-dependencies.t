Installed dune-package metadata can refer to libraries through public subsystem
fields that are not ordinary `requires`. A scoped package dependency currently
omits those referenced libraries.

  $ make_dune_project 3.24
  $ cat >>dune-project <<EOF
  > (package (name subsystem-root))
  > (package (name subsystem-runner))
  > EOF

  $ mkdir backend runner consumer
  $ cat >backend/dune <<'EOF'
  > (library
  >  (name backend)
  >  (public_name subsystem-root.backend)
  >  (modules ())
  >  (inline_tests.backend
  >   (runner_libraries subsystem-runner)
  >   (generate_runner (echo "let () = print_endline Runner.message"))))
  > EOF

  $ cat >runner/dune <<'EOF'
  > (library
  >  (name runner)
  >  (public_name subsystem-runner))
  > EOF
  $ cat >runner/runner.ml <<'EOF'
  > let message = "subsystem dependency"
  > EOF

  $ cat >consumer/dune-project <<'EOF'
  > (lang dune 3.24)
  > EOF
  $ cat >consumer/dune <<'EOF'
  > (library
  >  (name tested)
  >  (inline_tests (backend subsystem-root.backend)))
  > EOF
  $ echo 'let value = ()' >consumer/tested.ml

  $ cat >dune <<'EOF'
  > (rule
  >  (target result)
  >  (deps
  >   (package subsystem-root)
  >   (source_tree consumer))
  >  (action
  >   (with-stdout-to %{target}
  >    (chdir consumer (run %{bin:dune} runtest)))))
  > EOF

  $ dune build result 2>err
  [1]
  $ censor <err
  File "$PWD/_build/install/default/.packages/$DIGEST/lib/subsystem-root/dune-package", line 35, characters 20-36:
  35 |   (runner_libraries subsystem-runner)
                           ^^^^^^^^^^^^^^^^
  Error: Library "subsystem-runner" not found.
  -> required by
     _build/default/.tested.inline-tests/.t.eobjs/native/dune__exe__Main.cmx
  -> required by _build/default/.tested.inline-tests/inline-test-runner.exe
  -> required by alias runtest-tested in dune:3
  -> required by alias runtest in dune:1
