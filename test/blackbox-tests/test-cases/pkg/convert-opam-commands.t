Helper shell function that generates an opam file for a package:
  $ mkpkg() {
  >   name=$1
  >   mkdir -p mock-opam-repository/packages/$name/$name.0.0.1
  >   cat >mock-opam-repository/packages/$name/$name.0.0.1/opam
  > }

Helper shell function to generate a dune-project file and generate lockdir:
  $ solve_project() {
  >   cat >dune-project
  >   dune pkg lock --opam-repository-path=mock-opam-repository
  > }

Generate a mock opam repository
  $ mkdir -p mock-opam-repository
  $ cat >mock-opam-repository/repo <<EOF
  > opam-version: "2.0"
  > EOF

  $ mkpkg standard-dune <<EOF
  > opam-version: "2.0"
  > build: [
  >   ["dune" "subst"] {dev}
  >   [
  >     "dune"
  >     "build"
  >     "-p"
  >     name
  >     "-j"
  >     jobs
  >     "@install"
  >     "@runtest" {with-test}
  >     "@doc" {with-doc}
  >   ]
  > ]
  > install: [ make "install" ]
  > EOF

  $ mkpkg with-interpolation <<EOF
  > opam-version: "2.0"
  > build: [
  >   [
  >     "./configure"
  >     "--prefix=%{prefix}%"
  >     "--docdir=%{doc}%/ocaml"
  >   ]
  >   [make "-j%{jobs}%"]
  > ]
  > install: [make "install"]
  > EOF

Make sure we don't mess up percent signs that aren't part of variable interpolation syntax:
  $ mkpkg with-percent-sign <<EOF
  > opam-version: "2.0"
  > build: [ "printf" "%d" "42" ]
  > EOF

  $ mkpkg with-malformed-interpolation <<EOF
  > opam-version: "2.0"
  > build: [ "./configure" "--prefix=%{prefix" ]
  > EOF

  $ mkpkg variable-types <<EOF
  > opam-version: "2.0"
  > build: [
  >   ["echo" local_var]
  >   ["echo" _:explicit_local_var]
  >   ["echo" foo:package_var]
  >   ["echo" os-family]
  > ]
  > EOF

  $ solve_project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name x)
  >  (depends
  >   standard-dune
  >   with-interpolation
  >   with-percent-sign
  >   variable-types))
  > EOF
  Solution for dune.lock:
  standard-dune.0.0.1
  variable-types.0.0.1
  with-interpolation.0.0.1
  with-percent-sign.0.0.1
  

  $ cat dune.lock/standard-dune.pkg
  (version 0.0.1)
  
  (install
   (run %{make} install))
  
  (build
   (progn
    (run dune subst)
    (run dune build -p %{pkg-self:name} -j %{jobs} @install @runtest @doc)))

  $ cat dune.lock/with-interpolation.pkg
  (version 0.0.1)
  
  (install
   (run %{make} install))
  
  (build
   (progn
    (run ./configure --prefix=%{prefix} --docdir=%{doc}/ocaml)
    (run %{make} -j%{jobs})))

  $ cat dune.lock/with-percent-sign.pkg
  (version 0.0.1)
  
  (build
   (run printf %d 42))

  $ cat dune.lock/variable-types.pkg
  (version 0.0.1)
  
  (build
   (progn
    (run echo %{pkg-self:local_var})
    (run echo %{pkg-self:explicit_local_var})
    (run echo %{pkg:package_var:foo})
    (run echo %{os_family})))

  $ solve_project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name x)
  >  (depends with-malformed-interpolation))
  > EOF
  Error: Encountered malformed variable interpolation while processing commands
  for package with-malformed-interpolation.0.0.1.
  The variable interpolation:
  %{prefix
  The full command:
  "./configure" "--prefix=%{prefix"
  [1]
