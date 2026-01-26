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
mock_packages="mock-opam-repository/packages"

# this needs to be a function, because it might be called from a subdir
default_repo_path() {
  echo "file://$(pwd)/mock-opam-repository"
}

# Prints the directory containing the package target and source dirs within the
# _build directory.
get_build_pkg_dir() {
  local package_name=$1
  local digest
  digest=$($dune pkg print-digest "$package_name")
  local status=$?
  if [ "$status" -eq "0" ]; then
    echo "$pkg_root/$digest"
  else
    return 1
  fi
}

build_pkg() {
  local pkg=$1
  local prefix
  prefix="$(get_build_pkg_dir "$pkg")"
  local status=$?
  if [ "$status" -eq "0" ]; then
    $dune build "$prefix/target"
  else
    return 1
  fi
}

show_pkg() {
  local pkg=$1
  local prefix
  prefix="$(get_build_pkg_dir "$pkg")"
  find "$prefix" | sort | dune_cmd subst "$prefix" ""
}

strip_sandbox() {
  # we want to substitute it to $SANDBOX
  # shellcheck disable=SC2016
  dune_cmd subst '[^ ]*.sandbox/[^/]+' '$SANDBOX'
}

show_pkg_targets() {
  local pkg=$1
  local prefix
  prefix="$(get_build_pkg_dir "$pkg")/target"
  find "$prefix" | sort | dune_cmd subst "$prefix" ""
}

show_pkg_cookie() {
  local pkg=$1
  $dune internal dump "$(get_build_pkg_dir "$pkg")/target/cookie"
}

mkrepo() {
  mkdir -p $mock_packages
}

mkpkg() {
  local name=$1
  local version
  if [ "$#" -eq "1" ]
  then
    version="0.0.1"
  else
    version="$2"
  fi
  mkdir -p "$mock_packages/$name/$name.$version"
  echo 'opam-version: "2.0"' > "$mock_packages/$name/$name.$version/opam"
  cat >> "$mock_packages/$name/$name.$version/opam"
}

mk_ocaml() {
  local version="$1"
  local major
  major=$(echo "$version" | cut -d. -f1)
  local minor
  minor=$(echo "$version" | cut -d. -f2)
  local patch
  patch=$(echo "$version" | cut -d. -f3)
  local next
  next=$((patch + 1))
  local constraint="{>= \"$major.$minor.$patch~\" & < \"$major.$minor.$next~\"}"

  mkpkg ocaml "$version" <<- EOF
	depends: [
	  "ocaml-base-compiler" $constraint |
	  "ocaml-variants" $constraint
	]
	EOF

  mkpkg ocaml-base-compiler "$version" <<- EOF
	depends: [
	  "ocaml-compiler" $constraint
	]
	flags: compiler
	conflict-class: "ocaml-core-compiler"
	EOF

  mkpkg ocaml-variants "$version" <<- EOF
	depends: [
	  "ocaml-compiler" {= "$version"}
	]
	flags: compiler
	conflict-class: "ocaml-core-compiler"
	EOF

  mkpkg ocaml-compiler "$version" <<- EOF
	depends: [
	  "ocaml" {= "$version" & post}
	]
	EOF
}

set_pkg_to () {
  local value="${1}"
  if grep "(pkg .*)" dune-workspace > /dev/null; then
    dune_cmd substitute "(pkg .*)" "(pkg ${value})" dune-workspace
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
  dune_cmd delete "\(pkg" dune-workspace
}

add_mock_repo_if_needed() {
  # default, but can be overridden, e.g. if git is required
  local repo
  if [ "$#" -eq "0" ]; then
    repo=$(default_repo_path)
  else
    repo="$1"
  fi

  if [ ! -e dune-workspace ]
  then
      cat >dune-workspace <<- EOF
	(lang dune 3.20)
	(lock_dir
	 (repositories mock))
	(repository
	 (name mock)
	 (url "${repo}"))
	EOF
  else
    if ! grep '(name mock)' dune-workspace > /dev/null
    then
      # add the repo definition
      cat >>dune-workspace <<- EOF
	(repository
	 (name mock)
	 (url "${repo}"))
	EOF
 
      # reference the repo - only add lock_dir if no existing lock_dir references mock
      if ! grep '(repositories' dune-workspace | grep 'mock' > /dev/null
      then
        cat >>dune-workspace <<- EOF
	(lock_dir
	 (repositories mock))
	EOF
      fi
  
    fi
  fi
}

create_mock_repo() {
  # Always create a fresh workspace with mock repository configuration
  local repo="${1:-file://$(pwd)/mock-opam-repository}"
  cat >dune-workspace <<- EOF
	(lang dune 3.20)
	(lock_dir
	 (repositories mock))
	(repository
	 (name mock)
	 (url "${repo}"))
	EOF
}

make_lockpkg() {
  local pkg="$1"
  mkdir -p "${source_lock_dir}"
  local f="${source_lock_dir}/$pkg.pkg"
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

dune_pkg_lock_normalized() {
  local out
  local processed
  out="$(mktemp)"
  if dune pkg lock "$@" 2>> "${out}"; then
    processed="$(mktemp)"
    if [ "${DUNE_CONFIG__PORTABLE_LOCK_DIR:-}" = "disabled" ]; then
      cp "${out}" "${processed}"
    else
        awk '/Solution/{printf"%s:\n",$0;f=0};f{print};/Dependencies.*:/{f=1}' "${out}" \
        | dune_cmd subst '\(none\)' '(no dependencies to lock)' \
        > "${processed}"
    fi
    cat "${processed}"
  else
    processed="$(mktemp)"
      dune_cmd delete-between \
	      'The dependency solver failed to find a solution for the following platforms:' \
	      '\.\.\.with this error:' \
      < "${out}" \
      > "${processed}"
    cat "${processed}"
    return 1
  fi
}

solve_project() {
  cat >dune-project
  local repo
  repo=$(default_repo_path)
  add_mock_repo_if_needed "$repo"
  dune_pkg_lock_normalized "$@"
}

make_lockdir() {
  mkdir -p "${source_lock_dir}"
  cat > "${source_lock_dir}"/lock.dune <<- EOF
	(lang package 0.1)
	(repositories (complete true))
	EOF
}

make_project() {
  cat <<- EOF
	(lang dune 3.20)
	 (package
	  (name x)
	  (allow_empty)
	  (depends $@))
	EOF
}

print_source() {
  dune_cmd print-from 'source' \
	  < "${default_lock_dir}"/"$1".pkg \
  | dune_cmd print-until '^$' \
  | dune_cmd subst "$PWD" "PWD" \
  | tr '\n' ' ' \
  | tr -s " " \
  | dune_cmd subst '\s$' ''
}

solve() {
  make_project "$@" | solve_project dune.lock
}

# Pass a string of the form PACKAGE_NAME.PACKAGE_VERSION and replaces the
# hashes in all package digests matching the specified package with
# "DIGEST_HASH". Use this for packages whose lockfiles have different contents
# on different machines, such as lockfiles generated by expanding the "$PWD"
# variable.
sanitize_pkg_digest() {
  local pkg_name_and_version="${1}"
  dune_cmd subst "$pkg_name_and_version-[0-9a-f]*" "$pkg_name_and_version-DIGEST_HASH"
}
