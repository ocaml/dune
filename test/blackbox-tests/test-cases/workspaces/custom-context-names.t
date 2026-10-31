Validates custom workspace context names.


  $ cat > dune-workspace << EOF
  > (lang dune 3.13)
  > (context default)
  > (context
  >  (default
  >   (name log)))
  > EOF

  $ dune build
  File "dune-workspace", line 5, characters 8-11:
  5 |   (name log)))
              ^^^
  Error: "log" is an invalid context name.
  [1]

Context names must be portable path components.

  $ cat > dune-workspace << EOF
  > (lang dune 3.13)
  > (context default)
  > (context
  >  (default
  >   (name foo:bar)))
  > EOF

  $ dune build
  File "dune-workspace", line 5, characters 8-15:
  5 |   (name foo:bar)))
              ^^^^^^^
  Error: "foo:bar" is an invalid context name.
  [1]

  $ cat > dune-workspace << EOF
  > (lang dune 3.13)
  > (context default)
  > (context
  >  (default
  >   (name install)))
  > EOF
  $ dune build 2>&1 | grep "must not crash"
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  [1]

A valid name must still be unique across the workspace.

  $ cat > dune-workspace <<'EOF'
  > (lang dune 3.13)
  > (context (default (name repeated)))
  > (context (default (name repeated)))
  > EOF
  $ dune build
  File "dune-workspace", line 3, characters 9-34:
  3 | (context (default (name repeated)))
               ^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: second definition of build context "repeated"
  [1]

An explicit workspace path must exist, even when another workspace file is
present in the current directory.

  $ dune build --workspace missing-workspace
  Error: Workspace file
  $TESTCASE_ROOT/missing-workspace
  does not exist
  [1]

Invalid contexts below the install and private roots are rejected by rule
loading. Directory inspection also exercises install rule generation.

  $ make_dune_project 3.13
  $ cat > dune-workspace <<'EOF'
  > (lang dune 3.13)
  > (context default)
  > EOF

  $ dune describe targets --context install _build/install/missing 2> error.log
  $ awk '/^Description:/{printing=1;next} /^Raised/{printing=0} printing' error.log
    ("[gen_rules] did not specify rules for the context",
     { context_name = "install" })

  $ dune build _build/_private/missing/target 2> error.log
  [1]
  $ awk '/^Description:/{printing=1;next} /^Raised/{printing=0} printing' error.log
    ("[gen_rules] did not specify rules for the context",
     { context_name = "_private" })

  $ dune build _build/_private/log/target 2> error.log
  [1]
  $ awk '/^Description:/{printing=1;next} /^Raised/{printing=0} printing' error.log
    ("[gen_rules] did not specify rules for the context",
     { context_name = "_private" })

