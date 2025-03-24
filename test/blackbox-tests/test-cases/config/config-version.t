Tests verifying that the config file presents accurate error or warning
messages when a stanza is used with a version of dune which does not support
such a stanza.

Create a config file to use in the following tests. We will initialize the 
config with a version and a stanza incompatable with that version.

  $ export DUNE_CACHE_ROOT=$PWD/dune-cache
  $ touch dune-config
  $ cat >dune-config <<EOF
  > (lang dune 1.0)
  > (cache enabled)
  > EOF

Attempt to initialize a new project while an invaild stanza due to versioning
exists in the config.

  $ dune init proj test --config-file=dune-config
  File "$TESTCASE_ROOT/dune-config", line 2, characters 0-15:
  2 | (cache enabled)
      ^^^^^^^^^^^^^^^
  Error: 'cache' is only available since version 2.0 of the dune language.
  Please update your dune config file to have (lang dune 2.0).
  [1]

Update the dune configuration with a version that would support the 
'(cache enabled)' stanza and attempt a successful project initialization.

  $ cat >dune-config <<EOF
  > (lang dune 2.0)
  > (cache enabled)
  > EOF

  $ dune init proj test_vaild --config-file=dune-config
  Entering directory 'test_vaild'
  Success: initialized project component named test_vaild
  Leaving directory 'test_vaild'

Append an invaild stanza to the config file and attempt project initialzation.

  $ echo "(cache-check-probability 0.5)" >> dune-config
  $ dune init proj test --config-file=dune-config
  File "$TESTCASE_ROOT/dune-config", line 3, characters 0-29:
  3 | (cache-check-probability 0.5)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: 'cache-check-probability' is only available since version 2.7 of the
  dune language. Please update your dune config file to have (lang dune 2.7).
  [1]

