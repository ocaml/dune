Exercise the behaviour of "dune tools exec ocamllsp" when the lockdir
doesn't contain a lockfile for the "ocaml" package.

  $ . ../helpers.sh

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > 
  > (package
  >  (name foo)
  >  (allow_empty))
  > EOF

  $ make_lockdir

  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune tools exec ocamllsp
  Error: The lockdir doesn't contain a lockfile for the package "ocaml".
  Hint: Add a dependency on "ocaml" to one of the packages in dune-project and
  then run 'dune pkg lock'
  [1]
