----------------------------------------------------------------------------------
Handling ppx_runtime_libraries dependencies correctly

  $ cat >sdune <<'EOF'
  > #!/usr/bin/env bash
  > DUNE_SANDBOX=symlink dune "$@"
  > EOF
  $ chmod +x sdune

----------------------------------------------------------------------------------
* Incorrect cycle detection due to ppx_runtime_libraries (TODO: fix this bug!)

  $ cat >dune-project <<EOF
  > (lang dune 2.0)
  > (implicit_transitive_deps true)
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (name a)
  >  (modules a)
  >  (libraries b))
  > (library
  >  (name b)
  >  (kind ppx_rewriter)
  >  (modules b ppx)
  >  (libraries ppxlib)
  >  (ppx_runtime_libraries c))
  > (library
  >  (name c)
  >  (modules c)
  >  (libraries a))
  > EOF

  $ mkdir -p bin
  $ cat >bin/dune <<EOF
  > (executable
  >  (name main)
  >  (preprocess (pps b))
  >  (modules main))
  > EOF

  $ cat >a.ml <<EOF
  > include B
  > let a = B.b - 1
  > EOF

  $ cat >b.ml <<EOF
  > let b = 2
  > EOF

  $ cat >c.ml <<EOF
  > include A
  > let c = A.a + 2
  > EOF

  $ cat >bin/main.ml <<EOF
  > let () = Printf.printf "Should print 3: %d\n" [%get_c]
  > EOF

  $ ./sdune exec bin/main.exe
  Error: Dependency cycle detected between the following libraries:
     "a" in _build/default
  -> "b" in _build/default
  -> "c" in _build/default
  -> "a" in _build/default
  -> required by library "c" in _build/default
  -> required by executable main in bin/dune:2
  [1]

----------------------------------------------------------------------------------
* Make sure our usage of ppx_runtime_libraries is actually correct
TODO: Delete after fixing the test above

  $ cat >dune-project <<EOF
  > (lang dune 2.0)
  > (implicit_transitive_deps true)
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (name a)
  >  (modules a))
  > (library
  >  (name b)
  >  (kind ppx_rewriter)
  >  (modules b ppx)
  >  (libraries ppxlib)
  >  (ppx_runtime_libraries c))
  > (library
  >  (name c)
  >  (modules c)
  >  (libraries a))
  > EOF

  $ cat >a.ml <<EOF
  > let a = 2 - 1
  > EOF

  $ ./sdune exec bin/main.exe
  Should print 3: 3
