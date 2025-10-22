  $ . ../helpers.sh

Source helpers for creating another repo, etc.
  $ . ./helpers.sh

We setup a couple of different mock repositories. First, create
mock-opam-repository with a couple of packages

  $ mkrepo
  $ mkpkg foo "0.0.2"
  $ mkpkg bar <<EOF
  > depends: [ "foo" {>= "0.0.1"} ]
  > synopsis: "Mock bar package that depends on foo."
  > EOF
  $ mkpkg blah

Create dune-workspace with mock repo configured

  $ add_mock_repo_if_needed

Search for all packages

  $ dune pkg search
  - bar 0.0.1 Mock bar package that depends on foo.
  - blah 0.0.1 (no synopsis)
  - foo 0.0.2 (no synopsis)

Next, create other-opam-repository with a couple of conflicting packages

  $ mkrepo_other
  $ mkpkg_other baz
  $ mkpkg_other foo "0.0.1" <<EOF
  > synopsis: "Foo package in other repo with a synopsis, but lower version."
  > EOF
  $ mkpkg_other bar "0.0.1" <<EOF
  > depends: [ "foo" {>= "0.0.1"} ]
  > synopsis: "Mock bar package in other repo that depends on foo."
  > EOF
  $ mkpkg_other blah <<EOF
  > flags: [avoid-version]
  > synopsis: "Mock blah package that should be avoided!!"
  > EOF

Create a dune-workspace with both the mock repositories added

  $ create_dune_workspace_with_mock_repos

Search for all packages

  $ dune pkg search
  - bar 0.0.1 Mock bar package in other repo that depends on foo.
  - baz 0.0.1 (no synopsis)
  - blah 0.0.1 (no synopsis)
  - foo 0.0.2 (no synopsis)

Search with a query argument -- exact name

  $ dune pkg search bar
  - bar 0.0.1 Mock bar package in other repo that depends on foo.

Another search with a query argument - substring of package name

  $ dune pkg search ar
  - bar 0.0.1 Mock bar package in other repo that depends on foo.
