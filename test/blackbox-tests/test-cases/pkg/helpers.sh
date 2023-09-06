export DUNE_PKG_OVERRIDE_OCAML=1

dune="dune"

pkg_root="_build/_private/default/.pkg"

build_pkg() {
  $dune build _build/_private/default/.pkg/$1/target/
}

show_pkg() {
  find $pkg_root/$1 | sort | sed "s#$pkg_root/$1##"
}

show_pkg_targets() {
  find $pkg_root/$1/target | sort | sed "s#$pkg_root/$1/target##"
}

show_pkg_cookie() {
  $dune internal dump $pkg_root/$1/target/cookie
}

mkrepo() {
  mkdir -p mock-opam-repository
  cat >mock-opam-repository/repo <<EOF
opam-version: "2.0"
EOF
}

mock_packages="mock-opam-repository/packages"

mkpkg() {
  name=$1
  if [ "$#" -eq "1" ]
  then
    version="0.0.1"
  else
    version="$2"
  fi
  mkdir -p $mock_packages/$name/$name.$version
  cat >$mock_packages/$name/$name.$version/opam
}

solve_project() {
  cat >dune-project
  dune pkg lock --opam-repository-path=mock-opam-repository
}

make_lockdir() {
mkdir dune.lock
cat >dune.lock/lock.dune <<EOF
(lang package 0.1)
EOF
}
