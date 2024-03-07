Test that solver vars in filters are replaced by their values in filter
expressions in lockfiles.

  $ . ./helpers.sh
  $ mkrepo

Declare a package which refers to some variables:
  $ mkpkg a <<EOF
  > build: [
  >   ["dune" "subst"] {dev}
  >   [
  >     "dune"
  >     "build"
  >     "-p"
  >     name
  >     "-j"
  >     jobs
  >     "--foobar" { foo = "bar" }
  >     "@install"
  >     "@runtest" {with-test}
  >     "@doc" {with-doc}
  >   ]
  > ]
  > EOF

Solve the package using the default solver env:
  $ solve a
  Solution for dune.lock:
  - a.0.0.1
  $ cat dune.lock/a.pkg
  (version 0.0.1)
  
  (build
   (progn
    (when
     %{pkg-self:dev}
     (run dune subst))
    (run
     dune
     build
     -p
     %{pkg-self:name}
     -j
     %{jobs}
     (when
      (= %{pkg-self:foo} bar)
      --foobar)
     @install)))

Make a custom solver env:
  $ cat >dune-workspace <<EOF
  > (lang dune 3.8)
  > (lock_dir
  >  (path dune.lock)
  >  (repositories mock)
  >  (solver_env
  >   (dev false)
  >   (with-doc true)
  >   (foo bar)))
  > (context
  >  (default
  >   (name default)
  >   (lock_dir dune.lock)))
  > (repository
  >  (name mock)
  >  (source "file://$(pwd)/mock-opam-repository"))
  > EOF

Run the solver using the new env:
  $ solve a
  Solution for dune.lock:
  - a.0.0.1
  $ cat dune.lock/a.pkg
  (version 0.0.1)
  
  (build
   (run dune build -p %{pkg-self:name} -j %{jobs} --foobar @install @doc))
