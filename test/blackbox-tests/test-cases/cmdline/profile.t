The interaction and order of overriding DUNE_PROFILE, --profile, and --release.

Bug #4632

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > EOF

  $ dune build --verbose 2>&1 | grep "; profile"
   ; profile = Dev

  $ dune build --release --verbose 2>&1 | grep "; profile"
   ; profile = Release

  $ export DUNE_PROFILE=envvar

  $ dune build --verbose 2>&1 | grep "; profile"
   ; profile = User_defined "envvar"

  $ dune build --release --verbose 2>&1 | grep "; profile"
   ; profile = Release

  $ dune build --profile cmdline --verbose 2>&1 | grep "; profile"
   ; profile = User_defined "cmdline"
