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

  $ cat ${default_lock_dir}/foo.0.0.1.pkg
  (version 0.0.1)
  
  (depends
   (all_platforms (dep)))
  
  (depexts
   ((a) %{pkg-self:foobar})
   ((b) %{pkg:dep:nonexistent-var})
   ((c) %{pkg:dep:installed})
   ((d) %{pkg:nonexistent-pkg:installed}))
