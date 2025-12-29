The interaction and order of overriding DUNE_PROFILE, --profile, and --release.

Bug #4632

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > EOF

  $ runtest() {
  > dune build $@
  > dune trace cat | jq 'include "dune"; logs("Dune context") | .context.profile'
  > }

  $ runtest
  "Dev"

  $ runtest --release
  "Release"

  $ export DUNE_PROFILE=envvar

  $ runtest
  [
    "User_defined",
    "envvar"
  ]

  $ runtest --release
  "Release"

  $ runtest --profile cmdline
  [
    "User_defined",
    "cmdline"
  ]
