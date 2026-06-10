An argument whose filter is always false simplifies to the empty list (Nil).
Currently it is kept in the generated "run" action and re-encoded as
"(when false \"\")" in the lock directory, instead of being dropped. Repro for
#15099 (fixed in the follow-up PR).

Here "dropme" is filtered by "absent-pkg:installed". Since "absent-pkg" is not
in the solution, its "installed" variable is false, so the argument simplifies
to Nil.

  $ mkrepo

  $ mkpkg with-always-false-arg <<EOF
  > build: [
  >   [ "echo" "always" "dropme" { absent-pkg:installed } ]
  > ]
  > EOF

  $ solve with-always-false-arg
  Solution for dune.lock:
  - with-always-false-arg.0.0.1

The "(when false \"\")" in the run action below is the bug being tracked: the
always-false argument should be dropped entirely.

  $ cat dune.lock/with-always-false-arg.0.0.1.pkg
  (version 0.0.1)
  
  (build
   (all_platforms ((action (run echo always (when false ""))))))

