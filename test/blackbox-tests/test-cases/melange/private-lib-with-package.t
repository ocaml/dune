Test melange private libraries with `(package ..)` field

  $ cat > dune-project <<EOF
  > (lang dune 3.9)
  > (using melange 0.1)
  > (package (name foo))
  > EOF

Add a private library to package foo

  $ mkdir lib
  $ cat > lib/dune <<EOF
  > (library
  >  (name a)
  >  (package foo)
  >  (modes melange))
  > EOF

Cmj rules `--bs-package-output` should be `.` like public libraries (relative
to the dune file dir)

  $ dune rules --root . --format=json lib/.a.objs/melange/a.cmj |
  > jq -r 'include "dune"; .[] | ruleActionFlagValues("--bs-package-output")'
  .melange_src

Cmj rules should include `--bs-package-name` with the private mangled name

  $ dune rules --root . --format=json lib/.a.objs/melange/a.cmj |
  > jq -r 'include "dune"; .[] | ruleActionFlagValues("--bs-package-name")'
  foo.__private__.a
