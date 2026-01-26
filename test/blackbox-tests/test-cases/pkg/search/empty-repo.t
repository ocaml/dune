We use an empty repository with no packages for this test.

  $ mkrepo

Create a dune-workspace with a the mock repository added

  $ add_mock_repo_if_needed

Search with a query argument -- exact name

  $ dune pkg search foo
  Error: No packages found matching "foo".
  [1]

Another search with a query argument - substring of package name

  $ dune pkg search ar
  Error: No packages found matching "ar".
  [1]

Search with no results

  $ dune pkg search baz
  Error: No packages found matching "baz".
  [1]

Search for all packages

  $ dune pkg search '.*'
  Error: No packages found matching ".*".
  [1]
