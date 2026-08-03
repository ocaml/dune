An argument whose filter is always false simplifies to the empty list (Nil) and
is dropped from the generated "run" action, rather than being kept and
re-encoded as "(when false \"\")" in the lock directory. Regression test for
#15099.

Here "dropme" is filtered by "absent-pkg:installed". Since "absent-pkg" is not
in the solution, its "installed" variable is false, so the argument is dropped.

  $ mkrepo

  $ mkpkg with-always-false-arg <<EOF
  > build: [
  >   [ "echo" "always" "dropme" { absent-pkg:installed } ]
  > ]
  > EOF

  $ solve with-always-false-arg
  Solution for dune.lock:
  - with-always-false-arg.0.0.1

  $ cat dune.lock/with-always-false-arg.0.0.1.pkg
  (version 0.0.1)
  
  (build
   (all_platforms ((action (run echo always)))))
