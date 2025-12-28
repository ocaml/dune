Test target_exec with cross-compilation contexts.

Setup environment:

  $ unset OCAMLFIND_TOOLCHAIN
  $ unset OCAMLFIND_CONF

Create a simple executable that prints a message:

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > EOF

  $ cat > hello.ml <<EOF
  > let () =
  >   Printf.printf "---- HELLO START ----\n";
  >   Printf.printf "Hello from OCaml!\n";
  >   Printf.printf "PWD: %s\n" (Sys.getcwd ());
  >   Printf.printf "Args: %s\n" (String.concat " " (Array.to_list Sys.argv));
  >   Printf.printf "---- HELLO END ----\n";
  >   Printf.eprintf "\n";
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name hello))
  > 
  > (rule
  >  (alias runhello)
  >  (action (run ./hello.exe --hello-arg1 --hello-arg2)))
  > 
  > (rule
  >  (alias runecho)
  >  (action (run sh -c "echo \"Hello from context %{context_name}\"")))
  > EOF

Setup the test_toolchain:

  $ actualocamlc="$(command -v ocamlc)"

  $ mkdir -p etc/findlib.conf.d
  $ cat >etc/findlib.conf <<EOF
  > path=""
  > ocamlc="$PWD/notocamlc"
  > EOF
  $ cat >etc/findlib.conf.d/test.conf <<EOF
  > path(test_toolchain)=""
  > ocamlc(test_toolchain)="$PWD/notocamlc-test_toolchain"
  > EOF

  $ cat >notocamlc <<EOF
  > #!/usr/bin/env sh
  > $actualocamlc \$@
  > EOF
  $ cat >notocamlc-test_toolchain <<EOF
  > #!/usr/bin/env sh
  > $actualocamlc \$@
  > EOF
  $ chmod +x notocamlc notocamlc-test_toolchain

  $ export OCAMLFIND_CONF=$PWD/etc/findlib.conf

Create wrappers for testing

  $ mkdir -p bin
  $ cat > bin/test_toolchain_wrapper.sh <<'EOF'
  > #!/bin/sh
  > echo "=== TEST_TOOLCHAIN WRAPPER START ==="
  > echo "WRAPPER PWD: $PWD"
  > echo "WRAPPER executing: $@"
  > echo "=== WRAPPER EXEC ==="
  > exec "$@"
  > EOF

  $ chmod +x bin/test_toolchain_wrapper.sh

Build @runhello with -x test_toolchain without target_exec
(should use host binary from ../default/hello.exe)

  $ dune build @runhello -x test_toolchain
  ---- HELLO START ----
  Hello from OCaml!
  PWD: $TESTCASE_ROOT/_build/default
  Args: ./hello.exe --hello-arg1 --hello-arg2
  ---- HELLO END ----
  
  ---- HELLO START ----
  Hello from OCaml!
  PWD: $TESTCASE_ROOT/_build/default.test_toolchain
  Args: ../default/hello.exe --hello-arg1 --hello-arg2
  ---- HELLO END ----
  


Build with -x test_toolchain and --target-exec test_toolchain=test_toolchain_wrapper.sh
(should use target binary ./hello.exe wrapped with the wrapper)

  $ PATH="$PWD/bin:$PATH" dune build @runhello -x test_toolchain --target-exec test_toolchain=test_toolchain_wrapper.sh --force
  ---- HELLO START ----
  Hello from OCaml!
  PWD: $TESTCASE_ROOT/_build/default
  Args: ./hello.exe --hello-arg1 --hello-arg2
  ---- HELLO END ----
  
  === TEST_TOOLCHAIN WRAPPER START ===
  WRAPPER PWD: $TESTCASE_ROOT/_build/default.test_toolchain
  WRAPPER executing: $TESTCASE_ROOT/_build/default.test_toolchain/hello.exe --hello-arg1 --hello-arg2
  === WRAPPER EXEC ===
  ---- HELLO START ----
  Hello from OCaml!
  PWD: $TESTCASE_ROOT/_build/default.test_toolchain
  Args: $TESTCASE_ROOT/_build/default.test_toolchain/hello.exe --hello-arg1 --hello-arg2
  ---- HELLO END ----
  

Build with -x test_toolchain and --target-exec with arguments
(should use target binary ./hello.exe wrapped with the wrapper)

  $ cat > bin/wrapper_with_args.sh <<'EOF'
  > #!/bin/sh
  > echo "=== WRAPPER WITH ARGS START ==="
  > echo "WRAPPER: all args: $@"
  > echo "=== WRAPPER EXEC ==="
  > # Skip the wrapper's own argument and exec the rest
  > shift
  > shift
  > exec "$@"
  > EOF

  $ chmod +x bin/wrapper_with_args.sh

  $ PATH="$PWD/bin:$PATH" dune build @runhello -x test_toolchain --target-exec "test_toolchain=wrapper_with_args.sh --arg1 --arg2" --force
  ---- HELLO START ----
  Hello from OCaml!
  PWD: $TESTCASE_ROOT/_build/default
  Args: ./hello.exe --hello-arg1 --hello-arg2
  ---- HELLO END ----
  
  === WRAPPER WITH ARGS START ===
  WRAPPER: all args: --arg1 --arg2 $TESTCASE_ROOT/_build/default.test_toolchain/hello.exe --hello-arg1 --hello-arg2
  === WRAPPER EXEC ===
  ---- HELLO START ----
  Hello from OCaml!
  PWD: $TESTCASE_ROOT/_build/default.test_toolchain
  Args: $TESTCASE_ROOT/_build/default.test_toolchain/hello.exe --hello-arg1 --hello-arg2
  ---- HELLO END ----
  

Build with -x test_toolchain and --target-exec wih a binary outside of the sources
(should not use the wrapper)

  $ dune build @runecho -x test_toolchain --target-exec test_toolchain=test_toolchain_wrapper.sh --force
  Hello from context default
  Hello from context default.test_toolchain


Build with -x test_toolchain and a wrapper that is not in the path

  $ dune build @runhello -x test_toolchain --target-exec test_toolchain=test_toolchain_wrapper.sh --force
  ---- HELLO START ----
  Hello from OCaml!
  PWD: $TESTCASE_ROOT/_build/default
  Args: ./hello.exe --hello-arg1 --hello-arg2
  ---- HELLO END ----
  
  Error: Target exec wrapper test_toolchain_wrapper.sh could not be found in
  the path!
  -> required by alias runhello (context default.test_toolchain) in dune:4
  [1]

Build with an invalid --target_exec option

  $ dune build @runhello --target-exec native_wrapper.sh --force
  Error: --target-exec: invalid format
  [1]
