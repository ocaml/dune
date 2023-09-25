
. ../helpers.sh

solve() {
  solve_project <<EOF
(lang dune 3.11)
 (package
  (name x)
  (allow_empty)
  (depends $@))
EOF
}