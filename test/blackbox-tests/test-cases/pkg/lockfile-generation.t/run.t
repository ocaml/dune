Simple example of generating a lock file with Dune

Generate a `dune-project` file.
  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name lockfile_generation_test)
  >  (depends
  >    foo
  >    (bar (>= "0.3"))
  >   ))
  > EOF

Run the solver and generate a lock directory.
  $ dune pkg lock --opam-repository-path=mock-opam-repository
  Solution for dune.lock:
  bar.0.5.0
  baz.0.1.0
  foo.0.0.1
  

Helper to the name and contents of each file in the lock directory separated by
"---", sorting by filename for consistency.
  $ print_all() { find dune.lock -type f | sort | xargs -I{} sh -c "printf '{}:\n\n'; cat {}; printf '\n\n---\n\n'"; }

Print the contents of each file in the lockdir:
  $ print_all
  dune.lock/bar.pkg:
  
  (version 0.5.0)
  
  
  ---
  
  dune.lock/baz.pkg:
  
  (version 0.1.0)
  
  
  ---
  
  dune.lock/foo.pkg:
  
  (version 0.0.1)
  (deps baz bar)
  
  
  ---
  
  dune.lock/lock.dune:
  
  (lang package 0.1)
  
  
  ---
  

Run the solver again preferring oldest versions of dependencies:
  $ dune pkg lock --version-preference=oldest --opam-repository-path=mock-opam-repository
  Solution for dune.lock:
  bar.0.4.0
  baz.0.1.0
  foo.0.0.1
  

  $ print_all
  dune.lock/bar.pkg:
  
  (version 0.4.0)
  
  
  ---
  
  dune.lock/baz.pkg:
  
  (version 0.1.0)
  
  
  ---
  
  dune.lock/foo.pkg:
  
  (version 0.0.1)
  (deps baz bar)
  
  
  ---
  
  dune.lock/lock.dune:
  
  (lang package 0.1)
  
  
  ---
  

Regenerate the `dune-project` file introducing an unsatisfiable constraint.
  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name lockfile_generation_test)
  >  (depends
  >    foo
  >    (bar (>= "0.6"))
  >   ))
  > EOF

Run the solver again. This time it will fail.
  $ dune pkg lock --opam-repository-path=mock-opam-repository
  Error: Unable to solve dependencies in build context: default
  Can't find all required versions.
  Selected: baz.0.1.0 foo.0.0.1 lockfile_generation_test.dev
  - bar -> (problem)
      foo 0.0.1 requires >= 0.2
      lockfile_generation_test dev requires >= 0.6
      Rejected candidates:
        bar.0.5.0: Incompatible with restriction: >= 0.6
        bar.0.4.0: Incompatible with restriction: >= 0.6
        bar.0.0.1: Incompatible with restriction: >= 0.2
  [1]

