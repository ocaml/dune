Depexts with unknown variables should be filtered out at lock time.

  $ mkrepo
  $ mkpkg dep <<EOF
  > EOF
  $ mkpkg foo <<EOF
  > depends: [ "dep" ]
  > depexts: [
  >   [ "a" ] {foobar}
  >   [ "b" ] {dep:nonexistent-var}
  >   [ "c" ] {dep:installed}
  >   [ "d" ] {nonexistent-pkg:installed}
  > ]
  > EOF

  $ solve_project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name bar)
  >  (depends foo))
  > EOF
  Solution for dune.lock:
  - dep.0.0.1
  - foo.0.0.1

Only "c" should be kept since "dep" is in the solution and "installed" is a
standard package variable. Cases "a", "b", and "d" should be filtered:
- a: bare undefined variable
- b: package exists but variable is not standard
- d: package not in solution (CR-someday Alizter: currently not filtered)

  $ cat ${default_lock_dir}/foo.0.0.1.pkg
  (version 0.0.1)
  
  (depends
   (all_platforms (dep)))
  
  (depexts
   ((c) %{pkg:dep:installed})
   ((d) %{pkg:nonexistent-pkg:installed}))
