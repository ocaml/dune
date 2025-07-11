Exercise the "dune pkg enabled" command which checks whether package management
should be used.

  $ . ./helpers.sh

  $ mkrepo

  $ cat >dune-workspace <<EOF
  > (lang dune 3.20)
  > (lock_dir
  >  (path dune.lock)
  >  (repositories mock))
  > (lock_dir
  >  (path dune.other.lock)
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (allow_empty)
  >  (name foo))
  > EOF

When no lockdir is present pkg is not enabled:
  $ dune pkg enabled
  [1]

When the default lockdir is present pkg is enabled:
  $ dune pkg lock > /dev/null 2> /dev/null

  $ dune pkg enabled

  $ rm -r dune.lock

When a non-default lockdir is present, pkg is still enabled:
  $ dune pkg lock dune.other.lock > /dev/null 2> /dev/null
  $ dune pkg enabled
