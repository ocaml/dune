  $ . ./helpers.sh

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

Package for exercising opam filters on commands:
  $ mkpkg exercise-filters <<EOF
  > opam-version: "2.0"
  > build: [
  >   [ "echo" "a" ] { foo  }
  >   [ "echo" "b" ] { foo & bar }
  >   [ "echo" "c" ] { foo & bar & baz }
  >   [ "echo" "d" ] { foo | bar }
  >   [ "echo" "e" ] { foo | bar & baz }
  >   [ "echo" "f" ] { (foo | bar) & baz }
  >   [ "echo" "b" ] { foo = bar }
  >   [ "echo" "g" ] { version < "1.0" }
  >   [ "echo" "h" ] { with-test & ocaml:version < "5.0.0" }
  >   [ "echo" "i" ] { true }
  >   [ "echo" "j" ] { ! false }
  >   [ "echo" "k" ] { foo:installed }
  >   [ "echo" "l" ] { foo:version < "0.4" }
  >   [ "echo" "m" ] { foo+bar+baz:installed }
  > ]
  > EOF

Package with incorrect package variable conjunction where string was expected:
  $ mkpkg filter-error-invalid-conjunction <<EOF
  > opam-version: "2.0"
  > build: [
  >   [ "echo" "a" ] { foo+bar+baz:version < "0.4" }
  > ]
  > EOF

Package which has boolean where string was expected. This should be caught while parsing:
  $ mkpkg filter-error-bool-where-string-expected <<EOF
  > opam-version: "2.0"
  > build: [
  >   [ "echo" "a" ] { foo:version < (foo = bar) }
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
   (system "%{make} install"))
  
  (build
   (progn
    (when
     %{pkg-self:dev}
     (system "dune subst"))
    (system "dune build -p %{pkg-self:name} -j %{jobs} @install @runtest @doc")))

  $ cat dune.lock/with-interpolation.pkg
  (version 0.0.1)
  
  (install
   (system "%{make} install"))
  
  (build
   (progn
    (system "./configure --prefix=%{prefix} --docdir=%{doc}/ocaml")
    (system "%{make} -j%{jobs}")))

  $ cat dune.lock/with-percent-sign.pkg
  (version 0.0.1)
  
  (build
   (system "printf %d 42"))

  $ cat dune.lock/variable-types.pkg
  (version 0.0.1)
  
  (build
   (progn
    (system "echo %{pkg-self:local_var}")
    (system "echo %{pkg-self:explicit_local_var}")
    (system "echo %{pkg:package_var:foo}")
    (system "echo %{os_family}")))

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

  $ solve_project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name x)
  >  (depends exercise-filters))
  > EOF
  Solution for dune.lock:
  exercise-filters.0.0.1
  

  $ cat dune.lock/exercise-filters.pkg
  (version 0.0.1)
  
  (build
   (progn
    (when
     %{pkg-self:foo}
     (system "echo a"))
    (when
     (and %{pkg-self:foo} %{pkg-self:bar})
     (system "echo b"))
    (when
     (and
      (and %{pkg-self:foo} %{pkg-self:bar})
      %{pkg-self:baz})
     (system "echo c"))
    (when
     (or %{pkg-self:foo} %{pkg-self:bar})
     (system "echo d"))
    (when
     (or
      %{pkg-self:foo}
      (and %{pkg-self:bar} %{pkg-self:baz}))
     (system "echo e"))
    (when
     (and
      (or %{pkg-self:foo} %{pkg-self:bar})
      %{pkg-self:baz})
     (system "echo f"))
    (when
     (= %{pkg-self:foo} %{pkg-self:bar})
     (system "echo b"))
    (when
     (< %{pkg-self:version} 1.0)
     (system "echo g"))
    (when
     (and
      %{pkg-self:with-test}
      (< %{pkg:version:ocaml} 5.0.0))
     (system "echo h"))
    (when
     true
     (system "echo i"))
    (when
     (not false)
     (system "echo j"))
    (when
     %{pkg:installed:foo}
     (system "echo k"))
    (when
     (< %{pkg:version:foo} 0.4)
     (system "echo l"))
    (when
     (and %{pkg:installed:foo} %{pkg:installed:bar} %{pkg:installed:baz})
     (system "echo m"))))

  $ solve_project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name x)
  >  (depends filter-error-invalid-conjunction))
  > EOF
  Error: Expected string or identifier but found conjunction of identifiers:
  foo+bar+baz:version
  ...while processing commands for package:
  filter-error-invalid-conjunction.0.0.1
  Full filter: foo+bar+baz:version
  Note that name1+name2+name3:var is the conjunction of var for each of name1,
  name2 and name3, i.e it is equivalent to name1:var & name2:var & name3:var.
  [1]

  $ solve_project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name x)
  >  (depends filter-error-bool-where-string-expected))
  > EOF
  Error: At
  $TESTCASE_ROOT/mock-opam-repository/packages/filter-error-bool-where-string-expected/filter-error-bool-where-string-expected.0.0.1/opam:3:33-3:34::
  Parse error
  [1]

