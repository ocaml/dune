
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
  >   "no-deps-a" { os = linux }
  >   "no-deps-b" { arch = arm }
  > ]
  > EOF

A packgae which uses variables to determine its dependencies. Filter
expressions use boolean operators with variables in positions which would allow
them to be ignored under lazy evaluation. This is to clarify the behaviour of
the logic which stores solver vars in lockdir metadata in this case.
  $ mkpkg dynamic-deps-lazy 1.0 <<EOF
  > depends: [
  >   "no-deps-a" { os = linux | os-family = ubuntu }
  >   "no-deps-b" { arch = arm | os-version = "22.04" }
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

Solve packages with no variables set.
  $ solve_all
  Solution for dune.lock:
  - no-deps-a.1.0
  - no-deps-b.1.0
  - static-deps.1.0
  (lang package 0.1)
  
  (dependency_hash aea7daa72b636fa3a44973ec5e29d225)
  
  (repositories
   (complete false)
   (used))
  Solution for dune.lock:
  - dynamic-deps.1.0
  (lang package 0.1)
  
  (dependency_hash 6957fba0128609ffc98fac2561c329cb)
  
  (repositories
   (complete false)
   (used))
  
  (expanded_solver_variable_bindings
   (unset_variables os arch))
  Solution for dune.lock:
  - dynamic-deps-lazy.1.0
  (lang package 0.1)
  
  (dependency_hash 9675a3014e7e2db0f946b3ad2a95c037)
  
  (repositories
   (complete false)
   (used))
  
  (expanded_solver_variable_bindings
   (unset_variables os-version os-family os arch))

Make a workspace file which sets some of the variables.
  $ cat >dune-workspace <<EOF
  > (lang dune 3.8)
  > (context
  >  (default
  >   (name default)
  >   (solver_sys_vars
  >    (os linux)
  >    (arch arm))))
  > EOF

Solve the packages again, this time with the variables set.
  $ solve_all
  Solution for dune.lock:
  - no-deps-a.1.0
  - no-deps-b.1.0
  - static-deps.1.0
  (lang package 0.1)
  
  (dependency_hash aea7daa72b636fa3a44973ec5e29d225)
  
  (repositories
   (complete false)
   (used))
  Solution for dune.lock:
  - dynamic-deps.1.0
  (lang package 0.1)
  
  (dependency_hash 6957fba0128609ffc98fac2561c329cb)
  
  (repositories
   (complete false)
   (used))
  
  (expanded_solver_variable_bindings
   (variable_values
    (os linux)
    (arch arm)))
  Solution for dune.lock:
  - dynamic-deps-lazy.1.0
  (lang package 0.1)
  
  (dependency_hash 9675a3014e7e2db0f946b3ad2a95c037)
  
  (repositories
   (complete false)
   (used))
  
  (expanded_solver_variable_bindings
   (variable_values
    (os linux)
    (arch arm))
   (unset_variables os-version os-family))
