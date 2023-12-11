  $ . ./helpers.sh

Generate a mock opam repository
  $ mkdir -p mock-opam-repository
  $ cat >mock-opam-repository/repo <<EOF
  > opam-version: "2.0"
  > EOF

  $ mkpkg foo <<EOF
  > install: [
  >   ["echo" "package: %{name}%.%{version}%"]
  >   ["echo" "enable: %{enable}%"]
  >   ["echo" "installed: %{installed}%"]
  >   ["echo" "string selection: %{installed?foo:bar}%"]
  >   ["echo" "package conjunction: %{foo+bar+_:installed}%"]
  >   ["echo" "package conjunction string selection: %{foo+bar+_:installed?foo:bar}%"]
  > ]
  > EOF

  $ mkpkg bar <<EOF
  > install: [
  >   ["echo" "installed"] { installed }
  >   ["echo" "pinned"] { pinned }
  >   ["echo" "installed or pinned"] { installed | pinned }
  >   ["echo" "installed and pinned"] { installed & pinned }
  >   ["echo" "version greater than 0.1 (version is %{version}%)"] { version > "0.1" }
  >   ["echo" "version greater than 0 (version is %{version}%)"] { version > "0" }
  >   ["echo" "disjunction with some undefined vars"] { madeup | false | installed | madeup2 }
  >   ["echo" "conjunction with some undefined vars"] { ! (madeup & false & installed & madeup2) }
  >   ["echo" "check if variable 'madeup' is defined"] { ? madeup }
  >   ["echo" "check if variable 'installed' is defined"] { ? installed }
  > ]
  > EOF


  $ mkpkg baz <<EOF
  > install: [
  >   ["echo" "installed" { installed } "not installed" { ! installed }]
  >   ["echo" "madeup:installed" { madeup:installed } "not madeup:installed" { ! madeup:installed }]
  >   ["not-a-program" { ! (2 < 3) } "echo" "hello" ]
  >   ["echo" "madeup-defined" { ? madeup } "installed-defined" { ? installed } ]
  > ]
  > EOF


  $ mkpkg error1 <<EOF
  > install: [
  >   ["echo" "disjunction with all undefined or false vars"] { a | b | false | c | madeup:installed }
  > ]
  > EOF

  $ mkpkg error2 <<EOF
  > install: [
  >   ["echo" "conjunction with all undefined or true vars"] { a & b & true & c & installed }
  > ]
  > EOF

  $ mkpkg error3 <<EOF
  > install: [
  >   ["not-a-program" { ! (2 < 1) } "echo" "hello" ]
  > ]
  > EOF

  $ mkpkg error4 <<EOF
  > install: [
  >   ["not-a-program-%{name}%" { ! (2 < 1) } "echo" "hello" ]
  > ]
  > EOF

  $ build_single_package() {
  > solve_project <<EOF
  > (lang dune 3.11)
  > (package
  >  (name x)
  >  (depends
  >   $1))
  > EOF
  > build_pkg $1
  > }

  $ build_single_package foo
  Solution for dune.lock:
  - foo.0.0.1
  package: foo.0.0.1
  enable: enable
  installed: true
  string selection: foo
  package conjunction: false
  package conjunction string selection: bar
  $ build_single_package bar
  Solution for dune.lock:
  - bar.0.0.1
  installed
  installed or pinned
  version greater than 0 (version is 0.0.1)
  disjunction with some undefined vars
  conjunction with some undefined vars
  $ build_single_package baz
  Solution for dune.lock:
  - baz.0.0.1
  installed
  not madeup:installed
  hello
  
  $ build_single_package error1
  Solution for dune.lock:
  - error1.0.0.1
  File "dune.lock/error1.pkg", line 6, characters 3-16:
  6 |    %{pkg-self:a}
         ^^^^^^^^^^^^^
  Error: Undefined package variable "a"
  [1]
  $ build_single_package error2
  Solution for dune.lock:
  - error2.0.0.1
  File "dune.lock/error2.pkg", line 6, characters 3-16:
  6 |    %{pkg-self:a}
         ^^^^^^^^^^^^^
  Error: Undefined package variable "a"
  [1]
  $ build_single_package error3
  Solution for dune.lock:
  - error3.0.0.1
  File "dune.lock/error3.pkg", line 4, characters 6-19:
  4 |  (run not-a-program echo hello))
            ^^^^^^^^^^^^^
  Error: Program not-a-program not found in the tree or in PATH
   (context: default)
  [1]
  $ build_single_package error4
  Solution for dune.lock:
  - error4.0.0.1
  File "dune.lock/error4.pkg", line 4, characters 6-36:
  4 |  (run not-a-program-%{pkg-self:name} echo hello))
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Program not-a-program-error4 not found in the tree or in PATH
   (context: default)
  [1]
