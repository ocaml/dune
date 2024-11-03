
  $ . ./helpers.sh
  $ mkrepo

  $ mkpkg no-deps-a 1.0 <<EOF
  > EOF

  $ mkpkg no-deps-b 1.0 <<EOF
  > EOF

A package which doesn't use variables to determine its dependencies
  $ mkpkg static-deps 1.0 <<EOF
  > depends: [
  >   "no-deps-a"
  >   "no-deps-b"
  > ]
  > EOF

A packgae which uses variables to determine its dependencies
  $ mkpkg dynamic-deps 1.0 <<EOF
  > depends: [
  >   "no-deps-a" { os = "linux" }
  >   "no-deps-b" { arch = "arm" }
  > ]
  > EOF

A packgae which uses variables to determine its dependencies. Filter
expressions use boolean operators with variables in positions which would allow
them to be ignored under lazy evaluation. This is to clarify the behaviour of
the logic which stores solver vars in lockdir metadata in this case.
  $ mkpkg dynamic-deps-lazy 1.0 <<EOF
  > depends: [
  >   "no-deps-a" { os = "linux" | os-family = "ubuntu" }
  >   "no-deps-b" { arch = "arm" | os-version = "22.04" }
  > ]
  > EOF

  $ solve_all() {
  > 
  >  solve static-deps
  >  cat dune.lock/lock.dune
  > 
  >  solve dynamic-deps
  >  cat dune.lock/lock.dune
  > 
  >  solve dynamic-deps-lazy
  >  cat dune.lock/lock.dune
  > }

Make a workspace file which sets some of the variables.
  $ cat >dune-workspace <<EOF
  > (lang dune 3.8)
  > (lock_dir
  >  (path dune.lock)
  >  (repositories mock)
  >  (solver_env
  >   (os linux)
  >   (os-family dunefamily)
  >   (os-version 15)
  >   (arch arm)))
  > (context
  >  (default
  >   (name default)))
  > (repository
  >  (name mock)
  >  (source "file://$(pwd)/mock-opam-repository"))
  > EOF

Solve the packages again, this time with the variables set.
  $ solve_all
  Solution for dune.lock:
  - no-deps-a.1.0
  - no-deps-b.1.0
  - static-deps.1.0
  (lang package 0.1)
  
  (dependency_hash 3f813eed4e2e65a0b1d17fee9b738899)
  
  (repositories
   (complete false)
   (used))
  Solution for dune.lock:
  - dynamic-deps.1.0
  - no-deps-a.1.0
  - no-deps-b.1.0
  (lang package 0.1)
  
  (dependency_hash 2b84dc8b1f93a9cb3c8c060235c014a2)
  
  (repositories
   (complete false)
   (used))
  
  (expanded_solver_variable_bindings
   (variable_values
    (os linux)
    (arch arm)))
  Solution for dune.lock:
  - dynamic-deps-lazy.1.0
  - no-deps-a.1.0
  - no-deps-b.1.0
  (lang package 0.1)
  
  (dependency_hash dcccc0b378d9035f0f00a871c2d29359)
  
  (repositories
   (complete false)
   (used))
  
  (expanded_solver_variable_bindings
   (variable_values
    (os-version 15)
    (os-family dunefamily)
    (os linux)
    (arch arm)))

Test that variables referred to in filters on build and install commands are
stored in the lockdir metadata:
  $ mkpkg filtered-commands <<EOF
  > build: [
  >  [ "echo" "foo" ] { os = "linux" }
  >  [ "echo" "bar" ] { os = "macos" }
  >  [ "echo" "baz" ] { ! (? x) }
  > ]
  > install: [
  >  [ "echo" "qux" ] { arch = "arm" }
  > ]
  > EOF

  $ solve_project <<EOF
  > (lang dune 3.12)
  > (package
  >  (name foo)
  >  (depends filtered-commands))
  > EOF
  Solution for dune.lock:
  - filtered-commands.0.0.1

  $ cat dune.lock/filtered-commands.pkg
  (version 0.0.1)
  
  (install
   (run echo qux))
  
  (build
   (progn
    (run echo foo)
    (run echo baz)))
  $ cat dune.lock/lock.dune
  (lang package 0.1)
  
  (dependency_hash e99c6a04197fafe2e8b7153de21bba97)
  
  (repositories
   (complete false)
   (used))
  
  (expanded_solver_variable_bindings
   (variable_values
    (os linux)
    (arch arm))
   (unset_variables x))
