Test that solver vars in filters are replaced by their values in filter
expressions in lockfiles.

  $ mkrepo

Declare a non-dune package which refers to some variables. Using a non-dune
build system ensures the build commands are preserved in the lockfile so the
partial filter evaluation is visible.
  $ mkpkg a <<EOF
  > build: [
  >   ["./configure"] {dev}
  >   [
  >     "./make"
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
  $ cat ${default_lock_dir}/a.0.0.1.pkg
  (version 0.0.1)
  
  (build
   (all_platforms
    ((action
      (progn
       (when %{pkg-self:dev} (run ./configure))
       (run
        ./make
        (when (catch_undefined_var (= %{pkg-self:foo} bar) false) --foobar)
        @install))))))

Make a custom solver env:
  $ cat > dune-workspace <<EOF
  > (lang dune 3.20)
  > (pkg enabled)
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
  >  (url "file://$(pwd)/mock-opam-repository"))
  > EOF

Run the solver using the new env:
  $ solve a
  Solution for dune.lock:
  - a.0.0.1
  $ cat ${default_lock_dir}/a.0.0.1.pkg
  (version 0.0.1)
  
  (build
   (all_platforms
    ((action
      (run ./make --foobar @install @doc)))))
