The interaction and order of overriding DUNE_PROFILE, --profile, and --release.

Bug #4632

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > EOF

  $ runtest() {
  > dune build $@
  > dune trace cat | jq '.[] | select(.cat == "log" and .args.message == "Dune context") | .args.context | .[] | select(.[0] == "profile")'
  > }

  $ runtest
  [
    "profile",
    "Dev"
  ]

  $ runtest --release
  [
    "profile",
    "Release"
  ]

  $ export DUNE_PROFILE=envvar

  $ runtest
  [
    "profile",
    [
      "User_defined",
      "envvar"
    ]
  ]

  $ runtest --release
  [
    "profile",
    "Release"
  ]

  $ runtest --profile cmdline
  [
    "profile",
    [
      "User_defined",
      "cmdline"
    ]
  ]
