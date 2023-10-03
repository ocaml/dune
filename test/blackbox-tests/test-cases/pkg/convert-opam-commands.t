  $ . ./helpers.sh
  $ mkrepo

  $ mkpkg standard-dune <<EOF
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
  > build: [ "printf" "%d" "42" ]
  > EOF

  $ mkpkg with-malformed-interpolation <<EOF
  > build: [ "./configure" "--prefix=%{prefix" ]
  > EOF

  $ mkpkg variable-types <<EOF
  > build: [
  >   ["echo" local_var]
  >   ["echo" _:explicit_local_var]
  >   ["echo" foo:package_var]
  >   ["echo" os-family]
  > ]
  > EOF

Package for exercising opam filters on commands:
  $ mkpkg exercise-filters <<EOF
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
  > build: [
  >   [ "echo" "a" ] { foo+bar+baz:version < "0.4" }
  > ]
  > EOF

Package which has boolean where string was expected. This should be caught while parsing:
  $ mkpkg filter-error-bool-where-string-expected <<EOF
  > build: [
  >   [ "echo" "a" ] { foo:version < (foo = bar) }
  > ]
  > EOF

  $ solve standard-dune with-interpolation with-percent-sign variable-types
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
    (when
     %{pkg-self:dev}
     (run dune subst))
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
    (run echo %{pkg:foo:package_var})
    (run echo %{os_family})))

  $ solve with-malformed-interpolation
  Error: Encountered malformed variable interpolation while processing commands
  for package with-malformed-interpolation.0.0.1.
  The variable interpolation:
  %{prefix
  The full command:
  "./configure" "--prefix=%{prefix"
  [1]

  $ solve exercise-filters
  Solution for dune.lock:
  exercise-filters.0.0.1
  

  $ cat dune.lock/exercise-filters.pkg
  (version 0.0.1)
  
  (build
   (progn
    (when
     %{pkg-self:foo}
     (run echo a))
    (when
     (and %{pkg-self:foo} %{pkg-self:bar})
     (run echo b))
    (when
     (and
      (and %{pkg-self:foo} %{pkg-self:bar})
      %{pkg-self:baz})
     (run echo c))
    (when
     (or %{pkg-self:foo} %{pkg-self:bar})
     (run echo d))
    (when
     (or
      %{pkg-self:foo}
      (and %{pkg-self:bar} %{pkg-self:baz}))
     (run echo e))
    (when
     (and
      (or %{pkg-self:foo} %{pkg-self:bar})
      %{pkg-self:baz})
     (run echo f))
    (when
     (= %{pkg-self:foo} %{pkg-self:bar})
     (run echo b))
    (when
     (< %{pkg-self:version} 1.0)
     (run echo g))
    (when
     (and
      %{pkg-self:with-test}
      (< %{pkg:ocaml:version} 5.0.0))
     (run echo h))
    (when
     true
     (run echo i))
    (when
     (not false)
     (run echo j))
    (when
     %{pkg:foo:installed}
     (run echo k))
    (when
     (< %{pkg:foo:version} 0.4)
     (run echo l))
    (when
     (and %{pkg:foo:installed} %{pkg:bar:installed} %{pkg:baz:installed})
     (run echo m))))

  $ solve filter-error-invalid-conjunction
  Error: Expected string or identifier but found conjunction of identifiers:
  foo+bar+baz:version
  ...while processing commands for package:
  filter-error-invalid-conjunction.0.0.1
  Full filter: foo+bar+baz:version
  Note that name1+name2+name3:var is the conjunction of var for each of name1,
  name2 and name3, i.e it is equivalent to name1:var & name2:var & name3:var.
  [1]

  $ solve filter-error-bool-where-string-expected
  Error: At
  $TESTCASE_ROOT/mock-opam-repository/packages/filter-error-bool-where-string-expected/filter-error-bool-where-string-expected.0.0.1/opam:3:33-3:34::
  Parse error
  [1]

