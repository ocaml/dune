This test is about `binaries` in `env` stanzas in `dune-workspace` files.

  $ t () {
  >   cat > dune-workspace << EOF
  > (lang dune $1)
  > (env ($2 (binaries $3)))
  > EOF
  >   dune build
  > }

In the default context, this produces an error.

  $ t 3.1 _ x.exe
  File "dune-workspace", line 2, characters 0-26:
  2 | (env (_ (binaries x.exe)))
      ^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: 'binaries' in an 'env' stanza in a dune-workspace file is only
  available since version 3.2 of the dune language. Please update your
  dune-project file to have (lang dune 3.2).
  [1]

For explicit profiles too:

  $ t 3.1 dev x.exe
  File "dune-workspace", line 2, characters 0-28:
  2 | (env (dev (binaries x.exe)))
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: 'binaries' in an 'env' stanza in a dune-workspace file is only
  available since version 3.2 of the dune language. Please update your
  dune-project file to have (lang dune 3.2).
  [1]

When the profile is not selected, this is ignored but a warning is printed:

  $ t 3.1 other x.exe
  File "dune-workspace", line 2, characters 0-30:
  2 | (env (other (binaries x.exe)))
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Warning: 'binaries' in an 'env' stanza in a dune-workspace file is only
  available since version 3.2 of the dune language. Please update your
  dune-project file to have (lang dune 3.2).

With 3.2, this fixes the error.

In the default context:

  $ t 3.2 _ x.exe

And for explicit profiles:

  $ t 3.2 dev x.exe

And for another profile:

  $ t 3.2 other x.exe

Even in 3.2, this fails with pforms in this field.

  $ t 3.2 _ x%{ext_dll}.exe
  File "dune-workspace", line 2, characters 18-33:
  2 | (env (_ (binaries x%{ext_dll}.exe)))
                        ^^^^^^^^^^^^^^^
  Error: Variables are not supported in 'binaries' in an 'env' stanza in a
  dune-workspace file.
  [1]

And for explicit profiles:

  $ t 3.2 dev x%{ext_dll}.exe
  File "dune-workspace", line 2, characters 20-35:
  2 | (env (dev (binaries x%{ext_dll}.exe)))
                          ^^^^^^^^^^^^^^^
  Error: Variables are not supported in 'binaries' in an 'env' stanza in a
  dune-workspace file.
  [1]

And for another profile:

  $ t 3.2 other x%{ext_dll}.exe
  File "dune-workspace", line 2, characters 22-37:
  2 | (env (other (binaries x%{ext_dll}.exe)))
                            ^^^^^^^^^^^^^^^
  Error: Variables are not supported in 'binaries' in an 'env' stanza in a
  dune-workspace file.
  [1]
