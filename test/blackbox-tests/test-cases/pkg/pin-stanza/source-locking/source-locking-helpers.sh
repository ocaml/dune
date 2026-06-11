setup_source_locking_project() {
  mkrepo
  add_mock_repo_if_needed

  mkdir _dependency
  cat >_dependency/dune-project <<'EOF'
(lang dune 3.21)
(package
  (name dependency))
EOF
  write_dependency_version initial
  cat >_dependency/dune <<'EOF'
(library
 (public_name dependency))
EOF

  git -C _dependency init --initial-branch=main --quiet
  git -C _dependency add -A
  git -C _dependency commit -m "Initial" --quiet

  cat >main.ml <<'EOF'
print_endline Dependency.version
EOF

  cat >dune <<'EOF'
(executable
 (name main)
 (libraries dependency))
EOF
}

write_dependency_version() {
  local version="$1"
  cat >_dependency/dependency.ml <<EOF
let version = "$version"
EOF
}

commit_dependency() {
  local message="$1"
  git -C _dependency add -A
  git -C _dependency commit -m "$message" --quiet
}

write_pin_project() {
  local url="$1"
  cat >dune-project <<EOF
(lang dune 3.21)
(pin
 (url "$url")
 (package (name dependency)))
(package
 (name main)
 (depends dependency))
EOF
}
