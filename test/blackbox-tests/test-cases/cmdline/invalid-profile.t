Profile names are restricted to non-empty strings of letters, digits,
underscores and dashes, and the single underscore '_' is reserved as a
wildcard pattern in (env ...) stanzas. This test exercises invalid profile
names passed through the command-line, the DUNE_PROFILE environment variable,
the dune-workspace file, and the (env ...) stanza of a dune file.

  $ make_dune_project 3.13

A '*' is not a valid profile name.

  $ dune build --profile "*"
  dune: option '--profile': "*" is an invalid profile.
  Usage: dune build [OPTION]… [TARGET]…
  Try 'dune build --help' or 'dune --help' for more information.
  [1]

The empty string is not a valid profile name either.

  $ dune build --profile ""
  dune: option '--profile': "" is an invalid profile.
  Usage: dune build [OPTION]… [TARGET]…
  Try 'dune build --help' or 'dune --help' for more information.
  [1]

A profile name containing a space is rejected when set via DUNE_PROFILE.

  $ DUNE_PROFILE="foo bar" dune build
  dune: environment variable 'DUNE_PROFILE': "foo bar" is an invalid profile.
  Usage: dune build [OPTION]… [TARGET]…
  Try 'dune build --help' or 'dune --help' for more information.
  [1]

An invalid profile set in dune-workspace is rejected with a precise location.

  $ cat > dune-workspace <<EOF
  > (lang dune 3.13)
  > (profile "*")
  > EOF

  $ dune build
  File "dune-workspace", line 2, characters 9-12:
  2 | (profile "*")
               ^^^
  Error: "*" is an invalid profile.
  Profile names must be non-empty and can only contain letters, digits, '_' and
  '-'. The name '_' is reserved as a wildcard.
  [1]

  $ rm dune-workspace

An invalid profile name in an (env ...) stanza is also rejected, with the
same precise error message as in dune-workspace.

  $ cat > dune <<EOF
  > (env
  >  ("foo bar"
  >   (flags (:standard))))
  > EOF

  $ dune build
  File "dune", line 2, characters 2-11:
  2 |  ("foo bar"
        ^^^^^^^^^
  Error: "foo bar" is an invalid profile.
  Profile names must be non-empty and can only contain letters, digits, '_' and
  '-'. The name '_' is reserved as a wildcard.
  [1]

The validation happens at parse time, so an invalid profile name in an
(env ...) stanza is rejected even when the build is run with a different
profile that does not match the badly-named entry.

  $ cat > dune <<EOF
  > (env
  >  (dev
  >   (flags (:standard)))
  >  ("foo bar"
  >   (flags (:standard))))
  > EOF

  $ dune build --profile dev
  File "dune", line 4, characters 2-11:
  4 |  ("foo bar"
        ^^^^^^^^^
  Error: "foo bar" is an invalid profile.
  Profile names must be non-empty and can only contain letters, digits, '_' and
  '-'. The name '_' is reserved as a wildcard.
  [1]

  $ dune build --profile release
  File "dune", line 4, characters 2-11:
  4 |  ("foo bar"
        ^^^^^^^^^
  Error: "foo bar" is an invalid profile.
  Profile names must be non-empty and can only contain letters, digits, '_' and
  '-'. The name '_' is reserved as a wildcard.
  [1]

The wildcard '_' is reserved in (env ...) stanzas and cannot be used as a
profile name on the command line.

  $ dune build --profile _
  dune: option '--profile': "_" is an invalid profile.
  Usage: dune build [OPTION]… [TARGET]…
  Try 'dune build --help' or 'dune --help' for more information.
  [1]

Inside an (env ...) stanza, '_' continues to work as a wildcard pattern that
matches any profile.

  $ cat > dune <<EOF
  > (env
  >  (_
  >   (flags (:standard))))
  > EOF

  $ dune build

Valid profile names consist of letters, digits, underscores and dashes,
and are accepted both on the command line and in an (env ...) stanza.

  $ cat > dune <<EOF
  > (env
  >  (my-profile_1
  >   (flags (:standard))))
  > EOF

  $ dune build --profile my-profile_1
