  $ . ../helpers.sh

We setup a simple mock repository directory with a couple of packages.

  $ mkrepo
  $ mkpkg foo
  $ mkpkg bar <<EOF
  > depends: [ "foo" {>= "0.0.1"} ]
  > synopsis: "Mock bar package that depends on foo."
  > EOF

Create a dune-workspace with a the mock repository added

  $ add_mock_repo_if_needed

Search without any query arguments

  $ dune pkg search
  - bar 0.0.1 Mock bar package that depends on foo.
  - foo 0.0.1 (no synopsis)

Search with a query argument -- exact name

  $ dune pkg search foo
  - foo 0.0.1 (no synopsis)

Another search with a query argument - substring of package name, upper-cased
since the search is caseless.

  $ dune pkg search 'ar'
  - bar 0.0.1 Mock bar package that depends on foo.

Search with no results

  $ dune pkg search baz
  Error: No packages found matching "baz".
  [1]

Search for all packages

  $ dune pkg search
  - bar 0.0.1 Mock bar package that depends on foo.
  - foo 0.0.1 (no synopsis)
