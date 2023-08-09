Helper shell function that generates an opam file for a package:
  $ mkpkg() {
  >   name=$1
  >   mkdir -p mock-opam-repository/packages/$name/$name.0.0.1
  >   cat >mock-opam-repository/packages/$name/$name.0.0.1/opam
  > }

Helper shell function to generate a dune-project file and generate lockdir:
  $ solve_project() {
  >   cat >dune-project
  >   dune pkg lock --opam-repository-path=mock-opam-repository
  > }

Generate a mock opam repository
  $ mkdir -p mock-opam-repository
  $ cat >mock-opam-repository/repo <<EOF
  > opam-version: "2.0"
  > EOF

  $ mkpkg standard-dune <<EOF
  > opam-version: "2.0"
  > build: [
  >   ["dune" "subst"] {dev}
  >   [
  >     "dune"
  >     "build"
  >     "-p"
  >     name
  >     "-j"
  >     jobs
  >     "@install"
  >     "@runtest" {with-test}
  >     "@doc" {with-doc}
  >   ]
  > ]
  > install: [ make "install" ]
  > EOF

  $ mkpkg with-unknown-variable <<EOF
  > opam-version: "2.0"
  > build: [ fake "install" ]
  > EOF

  $ solve_project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name x)
  >  (depends standard-dune))
  > EOF
  Solution for dune.lock:
  standard-dune.0.0.1
  

  $ cat dune.lock/standard-dune.pkg
  (version 0.0.1)
  (install (run %{make} install))
  (build (progn (run dune subst) (run dune build -p %{name} -j %{jobs} @install @runtest @doc)))

  $ solve_project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name x)
  >  (depends with-unknown-variable))
  > EOF
  Error: Encountered unknown variable "fake" while processing commands for
  package with-unknown-variable.0.0.1.
  The full command:
  fake "install"
  [1]
