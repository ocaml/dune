Test variable filters on dependencies

Helper shell function that generates an opam file for a package:
  $ mkpkg() {
  >   name=$1
  >   mkdir -p mock-opam-repository/packages/$name/$name.0.0.1
  >   cat >mock-opam-repository/packages/$name/$name.0.0.1/opam
  > }

Generate a mock opam repository including some test dependencies:
  $ mkdir -p mock-opam-repository
  $ cat >mock-opam-repository/repo <<EOF
  > opam-version: "2.0"
  > EOF

  $ mkpkg foo-dependency <<EOF
  > opam-version: "2.0"
  > EOF

  $ mkpkg foo <<EOF
  > opam-version: "2.0"
  > depends: [
  >   "foo-dependency"
  > ]
  > EOF

  $ mkpkg depends-on-foo <<EOF
  > opam-version: "2.0"
  > depends: [
  >   "foo"
  > ]
  > EOF

  $ mkpkg depends-on-foo-with-test <<EOF
  > opam-version: "2.0"
  > depends: [
  >   "foo" {with-test}
  > ]
  > EOF

  $ mkpkg conflicts-with-foo <<EOF
  > opam-version: "2.0"
  > conflicts: [
  >   "foo"
  > ]
  > EOF

Helper shell function to generate a dune-project file and generate lockdir:
  $ solve_project() {
  >   cat >dune-project
  >   dune pkg lock --opam-repository-path=mock-opam-repository
  > }

Regular dependencies are resolved transitively:
  $ solve_project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name x)
  >  (depends depends-on-foo))
  > EOF
  Solution for dune.lock:
  depends-on-foo.0.0.1
  foo.0.0.1
  foo-dependency.0.0.1
  

Transitive test dependencies are not included:
  $ solve_project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name x)
  >  (depends depends-on-foo-with-test))
  > EOF
  Solution for dune.lock:
  depends-on-foo-with-test.0.0.1
  

Test dependencies of the project are included:
  $ solve_project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name x)
  >  (depends
  >   (foo :with-test)))
  > EOF
  Solution for dune.lock:
  foo.0.0.1
  foo-dependency.0.0.1
  

Test dependencies of test dependencies are excluded:
  $ solve_project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name x)
  >  (depends (depends-on-foo-with-test :with-test)))
  > EOF
  Solution for dune.lock:
  depends-on-foo-with-test.0.0.1
  

Conflicting packages can't be co-installed:
  $ solve_project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name x)
  >  (depends foo conflicts-with-foo))
  > EOF
  Error: Unable to solve dependencies in build context: default
  Can't find all required versions.
  Selected: foo.0.0.1 foo-dependency.0.0.1 x.dev
  - conflicts-with-foo -> (problem)
      Rejected candidates:
        conflicts-with-foo.0.0.1: Requires foo conflict with all versions
  [1]

Conflicting packages in transitive dependencies can't be co-installed:
  $ solve_project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name x)
  >  (depends depends-on-foo conflicts-with-foo))
  > EOF
  Error: Unable to solve dependencies in build context: default
  Can't find all required versions.
  Selected: depends-on-foo.0.0.1 foo.0.0.1 foo-dependency.0.0.1 x.dev
  - conflicts-with-foo -> (problem)
      Rejected candidates:
        conflicts-with-foo.0.0.1: Requires foo conflict with all versions
  [1]

Conflicts with transitive test dependencies don't affect the solution:
  $ solve_project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name x)
  >  (depends depends-on-foo-with-test conflicts-with-foo))
  > EOF
  Solution for dune.lock:
  conflicts-with-foo.0.0.1
  depends-on-foo-with-test.0.0.1
  

