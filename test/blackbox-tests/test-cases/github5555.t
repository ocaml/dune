This test is about `binaries` in `env` stanzas in `dune-workspace` files.

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (alias foo)
  >  (action (run %{bin:x})))
  > EOF

  $ cat >x <<EOF
  > #/bin/sh
  > echo foo
  > EOF

  $ chmod +x x

  $ t () {
  > cat > dune-workspace <<EOF
  > (lang dune $1)
  > (env ($2 (binaries $3)))
  > EOF
  > dune build @foo
  > }


# CR-someday rgrinberg: the messages below mention dune-project, but the issue
# is in workspace files.

In the default context, this produces an error.

  $ t 3.1 _ x
  File "dune-workspace", line 2, characters 0-22:
  2 | (env (_ (binaries x)))
      ^^^^^^^^^^^^^^^^^^^^^^
  Error: "binaries" in an "env" stanza in a dune-workspace file is only
  available since version 3.2 of the dune language. Please update your
  dune-project file to have (lang dune 3.2).
  [1]

For explicit profiles too:

  $ t 3.1 dev x
  File "dune-workspace", line 2, characters 0-24:
  2 | (env (dev (binaries x)))
      ^^^^^^^^^^^^^^^^^^^^^^^^
  Error: "binaries" in an "env" stanza in a dune-workspace file is only
  available since version 3.2 of the dune language. Please update your
  dune-project file to have (lang dune 3.2).
  [1]

When the profile is not selected, this is ignored but a warning is printed:

  $ t 3.1 other x
  File "dune-workspace", line 2, characters 0-26:
  2 | (env (other (binaries x)))
      ^^^^^^^^^^^^^^^^^^^^^^^^^^
  Warning: "binaries" in an "env" stanza in a dune-workspace file is only
  available since version 3.2 of the dune language. Please update your
  dune-project file to have (lang dune 3.2).
  File "dune", line 3, characters 14-22:
  3 |  (action (run %{bin:x})))
                    ^^^^^^^^
  Error: Program x not found in the tree or in PATH
   (context: default)
  [1]

With 3.2, this fixes the error.

In the default context:

  $ t 3.2 _ x
  Error: No rule found for .bin/x
  -> required by %{bin:x} at dune:3
  -> required by alias foo in dune:1
  [1]

And for explicit profiles:

  $ t 3.2 dev x
  Error: No rule found for .bin/x
  -> required by %{bin:x} at dune:3
  -> required by alias foo in dune:1
  [1]

And for another profile:

  $ t 3.2 other x
  File "dune", line 3, characters 14-22:
  3 |  (action (run %{bin:x})))
                    ^^^^^^^^
  Error: Program x not found in the tree or in PATH
   (context: default)
  [1]

Even in 3.2, this fails with pforms in this field.

  $ t 3.2 _ x%{ext_dll}
  File "dune-workspace", line 2, characters 18-29:
  2 | (env (_ (binaries x%{ext_dll})))
                        ^^^^^^^^^^^
  Error: Variables are not supported in "binaries" in an "env" stanza in a
  dune-workspace file.
  [1]

And for explicit profiles:

  $ t 3.2 dev x%{ext_dll}
  File "dune-workspace", line 2, characters 20-31:
  2 | (env (dev (binaries x%{ext_dll})))
                          ^^^^^^^^^^^
  Error: Variables are not supported in "binaries" in an "env" stanza in a
  dune-workspace file.
  [1]

And for another profile:

  $ t 3.2 other x%{ext_dll}
  File "dune-workspace", line 2, characters 22-33:
  2 | (env (other (binaries x%{ext_dll})))
                            ^^^^^^^^^^^
  Error: Variables are not supported in "binaries" in an "env" stanza in a
  dune-workspace file.
  [1]

Version checking is supposed to work even if (binaries) is empty.

  $ t 3.1 _ ""
  File "dune-workspace", line 2, characters 0-21:
  2 | (env (_ (binaries )))
      ^^^^^^^^^^^^^^^^^^^^^
  Error: "binaries" in an "env" stanza in a dune-workspace file is only
  available since version 3.2 of the dune language. Please update your
  dune-project file to have (lang dune 3.2).
  [1]
