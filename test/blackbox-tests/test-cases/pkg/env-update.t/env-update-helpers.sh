. ../helpers.sh

# [mktestpkg <package> <depends> <env_update_op>] makes an opam <package> that depends
# on <depends> and updated "env_var" using <env_update_op>
mktestpkg() {
  mkpkg $1 <<EOF
opam-version: "2.0"
depends: [ "$2" ]
setenv: [
  [ override = "$1" ]
  [ prepend_sep := "$1" ]
  [ prepend += "$1" ]
  [ append_sep =: "$1" ]
  [ append =+ "$1" ]
]
build: [
  [ "echo" "\n$1:" ]
  [ "sh" "-c" "echo \"   override: \$override\"" ]
  [ "sh" "-c" "echo \"prepend_sep: \$prepend_sep\"" ] 
  [ "sh" "-c" "echo \"    prepend: \$prepend\"" ]
  [ "sh" "-c" "echo \" append_sep: \$append_sep\"" ]
  [ "sh" "-c" "echo \"     append: \$append\"" ]
]
EOF
  echo $1
  grep -A6 setenv $mock_packages/$1/$1.0.0.1/opam | tail -n6 | head -n5
}

solve() {
  solve_project <<EOF
(lang dune 3.11)
 (package
  (name x)
  (allow_empty)
  (depends $1))
EOF
}

setup_test() {
  mkrepo
  mktestpkg package1 "" 
  mktestpkg package2 package1 
  mktestpkg package3 package2 
  solve package3 2> /dev/null
}

test() {
  setup_test
  override="initial value" \
  prepend_sep="initial value" \
  prepend="initial value" \
  append_sep="initial value" \
  append="initial value" \
  build_pkg package3 
}