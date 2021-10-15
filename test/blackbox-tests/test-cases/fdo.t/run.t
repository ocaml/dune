Prerequisites:
- external tool ocamlfdo that can be installed from opam
- linux perf (called by ocamlfdo)
- compiler version >= 4.10 (support for function
sections and split compilation at emit).

A workspace context can be used to build an executable using
feedback-direct optimizations (fdo). The name of the context is
determined from the name of the target executable for fdo and the
default switch name, unless a name field is provided explicitly.
It should work for both default and opam switches, but we don't have
a way to test opam switches.

This test should build all three contexts:

  $ dune build src/foo.exe --workspace dune-workspace.1

  $ ./_build/default/src/foo.exe
  <root>/_build/default/src/foo.exe: hello from fdo!

  $ ./_build/default-fdo-foo/src/foo.exe
  <root>/_build/default-fdo-foo/src/foo.exe: hello from fdo!

  $ ./_build/foofoo/src/foo.exe
  <root>/_build/foofoo/src/foo.exe: hello from fdo!

This is intended to fail

  $ dune build --workspace dune-workspace.2
  File "$TESTCASE_ROOT/dune-workspace.2", line 15, characters 9-66:
  15 | (context (default
  16 |            (name default-fdo-test2)
  17 |            ))
  Error: second definition of build context "default-fdo-test2"
  [1]

  $ dune build fdo --workspace dune-workspace.3
  File "$TESTCASE_ROOT/dune-workspace.3", line 15, characters 9-97:
  15 | (context (default
  16 |            (name default-fdo-test2)
  17 |            (fdo src/test2.exe)
  18 |            ))
  Error: second definition of build context "default-fdo-test2"
  [1]

Check that OCAMLFDO_USE_PROFILE is handled correctly.

We cannot check that ocamlfdo built with a profile produces a working
executable.  The reason is that profiles cannot be applied when
compiled code changes, for example when compiler versions change.  To
detect such changes, profiles contain digests from the original
executable the profile was collected from.

To check that ocamlfdo command was invoked by dune with or without
profile, as expected, the test profile contains a fake digest that is
guaranteed to fail if ocamlfdo attempts to load the profile.

  $ OCAMLFDO_USE_PROFILE=what-can-go-here dune build src/foo.exe --workspace dune-workspace.4
  Error: Failed to parse environment variable:
  OCAMLFDO_USE_PROFILE=what-can-go-here
  Permitted values: if-exists always never
  Default: if-exists
  [1]
  $ OCAMLFDO_USE_PROFILE=if-exists dune build src/foo.exe --workspace dune-workspace.4

  $ OCAMLFDO_USE_PROFILE=if-exists dune build src-with-profile/foo.exe --workspace dune-workspace.5  2>&1 | grep 'Error: '
  Error: Linear IR for Dune__exe__Foo from file .foo.eobjs/native/dune__exe__Foo.cmir-linear does not match the version of this IR used for creating the profiled binary.

  $ OCAMLFDO_USE_PROFILE=never dune build src/foo.exe --workspace dune-workspace.4

  $ OCAMLFDO_USE_PROFILE=never dune build src-with-profile/foo.exe --workspace dune-workspace.5

  $ OCAMLFDO_USE_PROFILE=always dune build src/foo.exe --workspace dune-workspace.4
  Error: OCAMLFDO_USE_PROFILE=always but profile file src/foo.exe.fdo-profile
  does not exist.
  [1]

  $ OCAMLFDO_USE_PROFILE=always dune build src-with-profile/foo.exe --workspace dune-workspace.5  2>&1 | grep 'Error: '
  Error: Linear IR for Dune__exe__Foo from file .foo.eobjs/native/dune__exe__Foo.cmir-linear does not match the version of this IR used for creating the profiled binary.


Check OCAMLFDO_FLAGS are passed on to "ocamlfdo opt"

  $ OCAMLFDO_FLAGS="-help" dune build src/foo.exe --workspace dune-workspace.4 2>&1 | head -n 4
      ocamlfdo src/.foo.eobjs/native/dune__exe__Foo.cmir-linear-fdo [fdo]
  Use a profile to optimize intermediate representation of the program.
  
    ocamlfdo opt [INPUT ...]

