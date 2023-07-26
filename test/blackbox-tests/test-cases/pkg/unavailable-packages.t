Helper shell function that generates an opam file for a package:
  $ mkpkg() {
  >   name=$1
  >   version=$2
  >   mkdir -p mock-opam-repository/packages/$name/$name.$version
  >   cat >mock-opam-repository/packages/$name/$name.$version/opam
  > }

Set up two build contexts: a default one for all systems and another just for macos.
  $ cat >dune-workspace <<EOF
  > (lang dune 3.8)
  > (context (default))
  > (context
  >  (default
  >   (name macos)
  >   (lock dune.macos.lock)
  >   (solver_env
  >    (sys
  >     (os macos)))))
  > EOF

Helper shell function to generate a dune-project file and generate lockdir for both contexts:
  $ solve_project() {
  >   cat >dune-project
  >   dune pkg lock --opam-repository-path=mock-opam-repository --context=default
  >   dune pkg lock --opam-repository-path=mock-opam-repository --context=macos
  > }

Generate a mock opam repository including some test dependencies:
  $ mkdir -p mock-opam-repository
  $ cat >mock-opam-repository/repo <<EOF
  > opam-version: "2.0"
  > EOF

A package which is only available on linux:
  $ mkpkg linux-only 0.0.1 <<EOF
  > opam-version: "2.0"
  > available: os = "linux"
  > EOF
  $ mkpkg linux-only 0.0.2 <<EOF
  > opam-version: "2.0"
  > available: os = "linux"
  > EOF

A package with three versions with only the middle version available on macos:
  $ mkpkg macos-sometimes 0.0.1 <<EOF
  > opam-version: "2.0"
  > available: os = "linux"
  > EOF
  $ mkpkg macos-sometimes 0.0.2 <<EOF
  > opam-version: "2.0"
  > EOF
  $ mkpkg macos-sometimes 0.0.3 <<EOF
  > opam-version: "2.0"
  > available: os = "linux"
  > EOF

A package with an undefined variable in its availability filter for 0.0.1, and
which is only available on linux for 0.0.2:
  $ mkpkg undefined-var 0.0.1 <<EOF
  > opam-version: "2.0"
  > available: xos = "linux"
  > EOF
  $ mkpkg undefined-var 0.0.2 <<EOF
  > opam-version: "2.0"
  > available: os = "linux"
  > EOF

A package whose availability filter resolves to a string:
  $ mkpkg availability-string 0.0.1 <<EOF
  > opam-version: "2.0"
  > available: "foo"
  > EOF
  $ mkpkg availability-string 0.0.2 <<EOF
  > opam-version: "2.0"
  > available: os
  > EOF

A package whose oldest and newest version is only available if with-test is false.
  $ mkpkg with-test-check 0.0.1 <<EOF
  > opam-version: "2.0"
  > available: ! with-test
  > EOF
  $ mkpkg with-test-check 0.0.2 <<EOF
  > opam-version: "2.0"
  > EOF
  $ mkpkg with-test-check 0.0.3 <<EOF
  > opam-version: "2.0"
  > available: ! with-test
  > EOF

No solution will be available on macos as all versions of this package are only
available on linux.
  $ solve_project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name x)
  >  (depends linux-only))
  > EOF
  Solution for dune.lock:
  linux-only.0.0.2
  
  Error: Unable to solve dependencies in build context: macos
  Can't find all required versions.
  Selected: x.dev
  - linux-only -> (problem)
      No usable implementations:
        linux-only.0.0.2: Availability condition not satisfied
        linux-only.0.0.1: Availability condition not satisfied
  [1]

The latest version of the package will be chosen on linux but the middle
version will be chosen on macos as that's the only version available on macos.
  $ solve_project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name x)
  >  (depends macos-sometimes))
  > EOF
  Solution for dune.lock:
  macos-sometimes.0.0.3
  
  Solution for dune.macos.lock:
  macos-sometimes.0.0.2
  

A warning will be printed as the undefined-var.0.0.1 package has an undefined
variable in its `available` filter. The undefined-var.0.0.2 package has a valid
`available` filter but is only available on linux.
  $ solve_project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name x)
  >  (depends undefined-var))
  > EOF
  Solution for dune.lock:
  undefined-var.0.0.2
  
  Error: Unable to solve dependencies in build context: macos
  Can't find all required versions.
  Selected: x.dev
  - undefined-var -> (problem)
      No usable implementations:
        undefined-var.0.0.2: Availability condition not satisfied
        undefined-var.0.0.1: Availability condition not satisfied
  [1]

Warnings will be printed and no solution will be found as the availability
filter resolves to a string instead of to a boolean.
  $ solve_project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name x)
  >  (depends availability-string))
  > EOF
  Error: Unable to solve dependencies in build context: default
  Can't find all required versions.
  Selected: x.dev
  - availability-string -> (problem)
      No usable implementations:
        availability-string.0.0.2: Availability condition not satisfied
        availability-string.0.0.1: Availability condition not satisfied
  Error: Unable to solve dependencies in build context: macos
  Can't find all required versions.
  Selected: x.dev
  - availability-string -> (problem)
      No usable implementations:
        availability-string.0.0.2: Availability condition not satisfied
        availability-string.0.0.1: Availability condition not satisfied
  [1]

The middle version will be picked as this is the only one available if
with-test is set. This exercises that we can handle flags in the available
filter.
  $ solve_project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name x)
  >  (depends with-test-check))
  > EOF
  Solution for dune.lock:
  with-test-check.0.0.2
  
  Solution for dune.macos.lock:
  with-test-check.0.0.2
  
