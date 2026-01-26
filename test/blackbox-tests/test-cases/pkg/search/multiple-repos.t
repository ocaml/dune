Source helpers for creating another repo, etc.

We setup a couple of different mock repositories. First, create
mock-opam-repository with a couple of packages

  $ mkrepo
  $ mkpkg foo
  $ mkpkg bar <<EOF
  > depends: [ "foo" {>= "0.0.1"} ]
  > synopsis: "Mock bar package that depends on foo."
  > EOF

Next, create other-opam-repository with a package

  $ mkrepo_other
  $ mkpkg_other baz

Create a dune-workspace with both the mock repositories added

  $ create_dune_workspace_with_mock_repos

Search for all packages

  $ dune pkg search
  - bar 0.0.1 Mock bar package that depends on foo.
  - baz 0.0.1 (no synopsis)
  - foo 0.0.1 (no synopsis)

Search with a query argument -- exact name

  $ dune pkg search foo
  - foo 0.0.1 (no synopsis)

Another search with a query argument - substring of package name

  $ dune pkg search ar
  - bar 0.0.1 Mock bar package that depends on foo.

Search for package in other repo

  $ dune pkg search baz
  - baz 0.0.1 (no synopsis)

Search with results from both repos

  $ dune pkg search ba
  - bar 0.0.1 Mock bar package that depends on foo.
  - baz 0.0.1 (no synopsis)

Search with no results

  $ dune pkg search dune
  Error: No packages found matching "dune".
  [1]
