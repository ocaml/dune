Test variable filters on dependencies

  $ . ./helpers.sh
  $ mkrepo

  $ mkpkg foo-dependency <<EOF
  > EOF

  $ mkpkg foo <<EOF
  > depends: [
  >   "foo-dependency"
  > ]
  > EOF

  $ mkpkg depends-on-foo <<EOF
  > depends: [
  >   "foo"
  > ]
  > EOF

  $ mkpkg depends-on-foo-with-test <<EOF
  > depends: [
  >   "foo" {with-test}
  > ]
  > EOF

  $ mkpkg conflicts-with-foo <<EOF
  > conflicts: [
  >   "foo"
  > ]
  > EOF

Regular dependencies are resolved transitively:
  $ solve depends-on-foo
  Solution for dune.lock:
  - depends-on-foo.0.0.1
  - foo.0.0.1
  - foo-dependency.0.0.1

Transitive test dependencies are not included:
  $ solve depends-on-foo-with-test
  Solution for dune.lock:
  - depends-on-foo-with-test.0.0.1

Test dependencies of the project are included:
  $ solve "(foo :with-test)"
  Solution for dune.lock:
  - foo.0.0.1
  - foo-dependency.0.0.1

Test dependencies of test dependencies are excluded:
  $ solve "(depends-on-foo-with-test :with-test)"
  Solution for dune.lock:
  - depends-on-foo-with-test.0.0.1

Conflicting packages can't be co-installed:
  $ solve foo conflicts-with-foo
  Error: Unable to solve dependencies for the following lock directories:
  Lock directory dune.lock:
  Can't find all required versions.
  Selected: foo.0.0.1 foo-dependency.0.0.1 x.dev
  - conflicts-with-foo -> (problem)
      Rejected candidates:
        conflicts-with-foo.0.0.1: Requires foo conflict with all versions
  [1]

Conflicting packages in transitive dependencies can't be co-installed:
  $ solve depends-on-foo conflicts-with-foo
  Error: Unable to solve dependencies for the following lock directories:
  Lock directory dune.lock:
  Can't find all required versions.
  Selected: depends-on-foo.0.0.1 foo.0.0.1 foo-dependency.0.0.1 x.dev
  - conflicts-with-foo -> (problem)
      Rejected candidates:
        conflicts-with-foo.0.0.1: Requires foo conflict with all versions
  [1]

Conflicts with transitive test dependencies don't affect the solution:
  $ solve depends-on-foo-with-test conflicts-with-foo
  Solution for dune.lock:
  - conflicts-with-foo.0.0.1
  - depends-on-foo-with-test.0.0.1

