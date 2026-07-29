Validation must evaluate a local package's dependency formula with the
package's own name and version bound as self variables, the way the solver
does. Otherwise a constraint referring to the package's own version
evaluates to false and the dependency it guards is silently dropped,
weakening validation.

  $ mkrepo
  $ mkpkg foo 1.2.3

The local package constrains its dependency to its own version and the
solver resolves it by binding the self variables:

  $ solve_project <<EOF
  > (lang dune 3.20)
  > (version 1.2.3)
  > (package
  >  (name self)
  >  (allow_empty)
  >  (depends (foo (= :version))))
  > EOF
  Solution for dune.lock:
  - foo.1.2.3

Swap the locked dependency for a different version, violating the
constraint. Validation evaluates the formula without the self bindings,
which drops the version constraint instead of binding the package's own
version, and the broken lockdir is accepted:

  $ sed -i.bak 's/1\.2\.3/9.9.9/' dune.lock/foo.1.2.3.pkg
  $ mv dune.lock/foo.1.2.3.pkg dune.lock/foo.9.9.9.pkg
  $ dune pkg validate-lockdir
