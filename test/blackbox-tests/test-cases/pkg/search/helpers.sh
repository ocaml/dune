. ../helpers.sh

mkrepo_other() {
  local mock_packages
  mock_packages="other-opam-repository/packages"
  mkrepo
}

mkpkg_other() {
  local mock_packages
  mock_packages="other-opam-repository/packages"
  mkpkg "$@"
}

create_dune_workspace_with_mock_repos() {
  # Always create a fresh workspace with mock repository configuration
  repo1="file://$(pwd)/mock-opam-repository"
  repo2="file://$(pwd)/other-opam-repository"
  cat >dune-workspace <<EOF
(lang dune 3.20)
(pkg enabled)
(lock_dir
 (repositories other mock))
(repository
 (name mock)
 (url "${repo1}"))
(repository
 (name other)
 (url "${repo2}"))
EOF
}

mk_multiple_packages() {
  packages="dune utop merlin ocamlformat ppxlib yojson cmdliner eio alcotest core base bos notty js_of_ocaml atdgen owl re reason spectre"
  versions="0.2 0.3 0.5 1.0 1.3 2.0 3.0~alpha1"
  for pkg in $packages;
  do
    for version in $versions;
    do
      if [ "${#pkg}" -gt 4 ]; then
        prefix="This is just a very very very very very long synopsis for the excellent"
      else
        prefix="Short synopsis for"
      fi
      mkpkg "$pkg" "$version" <<EOF
depends: [ "ocaml" {>= "4.12.0"} ]
synopsis: "$prefix package: '$pkg' version: '$version'"
EOF
    done
  done
}
