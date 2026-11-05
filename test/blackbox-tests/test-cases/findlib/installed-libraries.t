List installed libraries, including META subpackages and absent versions.
Use an empty findlib configuration to exclude installed packages. Filter out
compiler-provided libraries from the available list.

  $ make_dune_project 3.22
  $ mkdir -p packages/testpkg packages/testpkg_broken packages/testpkg_hidden
  $ export OCAMLPATH="$PWD/packages"
  $ touch findlib.conf
  $ export OCAMLFIND_CONF="$PWD/findlib.conf"
  $ cat > packages/testpkg/META <<'EOF'
  > version = "1.2.3"
  > package "child" (
  >   version = "2.0"
  > )
  > package "unversioned" ()
  > EOF
  $ cat > packages/testpkg_hidden/META <<'EOF'
  > exists_if = "enabled"
  > EOF
  $ echo 'version = "0.1"' > packages/META.testpkg_flat
  $ touch packages/testpkg_broken/META
  $ echo '(lang dune invalid)' > packages/testpkg_broken/dune-package
  $ dune installed-libraries | grep '^testpkg' \
  > | dune_cmd subst ' +\(version:' ' (version:'
  testpkg (version: 1.2.3)
  testpkg.child (version: 2.0)
  testpkg.unversioned (version: n/a)
  testpkg_flat (version: 0.1)

Unavailable packages distinguish malformed dune-package files from unsatisfied
exists_if conditions, and are sorted together by name.

  $ dune installed-libraries --not-available | grep '^testpkg' \
  > | dune_cmd subst ' +->' ' ->'
  testpkg_broken -> invalid dune file
  testpkg_hidden -> unsatisfied 'exists_if'

Making the exists_if file appear makes the hidden library available.

  $ touch packages/testpkg_hidden/enabled
  $ dune installed-libraries | grep '^testpkg' \
  > | dune_cmd subst ' +\(version:' ' (version:'
  testpkg (version: 1.2.3)
  testpkg.child (version: 2.0)
  testpkg.unversioned (version: n/a)
  testpkg_flat (version: 0.1)
  testpkg_hidden (version: n/a)
  $ dune installed-libraries --na | grep '^testpkg' \
  > | dune_cmd subst ' +->' ' ->'
  testpkg_broken -> invalid dune file

An empty unavailable set prints nothing (including no heading).

  $ rm -r packages/testpkg_broken
  $ dune installed-libraries --not-available

A findlib search path that is a file is an error, not an empty search directory.

  $ touch not-a-directory
  $ OCAMLPATH="$PWD/not-a-directory" dune installed-libraries
  Error: Unable to read directory
  $TESTCASE_ROOT/not-a-directory
  for findlib package
  Reason: Not a directory
  [1]
