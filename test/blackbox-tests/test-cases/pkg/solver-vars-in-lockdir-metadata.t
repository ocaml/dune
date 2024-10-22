
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
  
  (dependency_hash ad6f4fff9120e8ab448eb83ca635a86d)
  
  (repositories
   (complete false)
   (used))
  Solution for dune.lock:
  - dynamic-deps.1.0
  - no-deps-a.1.0
  - no-deps-b.1.0
  (lang package 0.1)
  
  (dependency_hash 2298a16a57f43cef6b992dfeb7152cd0)
  
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
  
  (dependency_hash b819d902548dbaa50ba27ebca236bd53)
  
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
  
  (dependency_hash 9e7a69aa395c7c01337dcd2d4468aae5)
  
  (repositories
   (complete false)
   (used))
  
  (expanded_solver_variable_bindings
   (variable_values
    (os linux)
    (arch arm))
   (unset_variables x))
