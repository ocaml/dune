export XDG_CACHE_HOME="$PWD/.cache"

# Set the default platform for the purposes of solving dependencies so that the
# output of tests is platform-independent.
export DUNE_CONFIG__OS=linux
export DUNE_CONFIG__ARCH=x86_64
export DUNE_CONFIG__OS_FAMILY=debian
export DUNE_CONFIG__OS_DISTRIBUTION=ubuntu
export DUNE_CONFIG__OS_VERSION=24.11
export DUNE_CONFIG__SYS_OCAML_VERSION=5.4.0+fake

dune="dune"

pkg_root="_build/_private/default/.pkg"

default_lock_dir="dune.lock"
source_lock_dir="${default_lock_dir}"

build_pkg() {
  $dune build $pkg_root/$1/target/
}

show_pkg() {
  find $pkg_root/$1 | sort | sed "s#$pkg_root/$1##"
}

strip_sandbox() {
  sed -E 's#[^ ]*.sandbox/[^/]+#$SANDBOX#g'
}

show_pkg_targets() {
  find $pkg_root/$1/target | sort | sed "s#$pkg_root/$1/target##"
}

show_pkg_cookie() {
  $dune internal dump $pkg_root/$1/target/cookie
}

mock_packages="mock-opam-repository/packages"

mkrepo() {
  mkdir -p $mock_packages
}

mkpkg() {
  name=$1
  if [ "$#" -eq "1" ]
  then
    version="0.0.1"
  else
    version="$2"
  fi
  mkdir -p $mock_packages/$name/$name.$version
  echo 'opam-version: "2.0"' > $mock_packages/$name/$name.$version/opam
  cat >>$mock_packages/$name/$name.$version/opam
}

set_pkg_to () {
  local value="${1}"
  if grep "(pkg .*)" dune-workspace > /dev/null; then
    sed -i.bak "s/(pkg .*)/(pkg ${value})/" dune-workspace
  else
    echo "(pkg ${value})" >> dune-workspace
  fi
}

enable_pkg() {
  set_pkg_to "enabled"
}

disable_pkg() {
  set_pkg_to "disabled"
}

unset_pkg() {
  sed -i.bak "/(pkg/d" dune-workspace
}

add_mock_repo_if_needed() {
  # default, but can be overridden, e.g. if git is required
  repo="${1:-file://$(pwd)/mock-opam-repository}"

  if [ ! -e dune-workspace ]
  then
      cat >dune-workspace <<EOF
(lang dune 3.20)
(pkg enabled)
(lock_dir
 (repositories mock))
(repository
 (name mock)
 (url "${repo}"))
EOF
  else
    if ! grep '(name mock)' > /dev/null dune-workspace
    then
      # add the repo definition
      cat >>dune-workspace <<EOF
(repository
 (name mock)
 (url "${repo}"))
EOF
 
      # reference the repo
      if grep -s '(repositories'
      then
        sed -i '' -e 's/(repositories \(.*\))/(repositories mock \1)/' dune-workspace
      else
        cat >>dune-workspace <<EOF
(lock_dir
 (repositories mock))
EOF
      fi
  
    fi
  fi
}

create_mock_repo() {
  # Always create a fresh workspace with mock repository configuration
  repo="${1:-file://$(pwd)/mock-opam-repository}"
  cat >dune-workspace <<EOF
(lang dune 3.20)
(pkg enabled)
(lock_dir
 (repositories mock))
(repository
 (name mock)
 (url "${repo}"))
EOF
}

make_lockpkg() {
  mkdir -p "${source_lock_dir}"
  local f="${source_lock_dir}/$1.pkg"
  cat > "$f"
}

append_to_lockpkg() {
  local pkg="${1}"
  cat >> "${source_lock_dir}/${pkg}.pkg"
}

make_lockpkg_file() {
  local pkg="${1}"
  local filename="${2}"
  mkdir -p "${source_lock_dir}/${pkg}.files"
  cat > "${source_lock_dir}/${pkg}.files/${filename}"
}

solve_project() {
  cat >dune-project
  add_mock_repo_if_needed
  dune pkg lock $@
}

make_lockdir() {
  mkdir -p "${source_lock_dir}"
  cat > "${source_lock_dir}"/lock.dune <<EOF
(lang package 0.1)
(repositories (complete true))
EOF
}

make_project() {
  cat <<EOF
(lang dune 3.20)
 (package
  (name x)
  (allow_empty)
  (depends $@))
EOF
}

print_source() {
  cat "${default_lock_dir}"/"$1".pkg | sed -n "/source/,//p" | sed "s#$PWD#PWD#g" | tr '\n' ' '| tr -s " "
}

solve() {
  make_project $@ | solve_project
}
