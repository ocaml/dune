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

  $ dune rules lib/.a.objs/melange/a.cmj |
  > grep -e "--bs-package-output" --after-context=1
      --bs-package-output
      .

Cmj rules should include `--bs-package-name` with the private mangled name

  $ dune rules lib/.a.objs/melange/a.cmj |
  > grep -A1 -e "--bs-package-name"
      --bs-package-name
      foo.__private__.a


