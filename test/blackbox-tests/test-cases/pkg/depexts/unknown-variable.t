Solving with an unknown variable on depexts:

  $ mkrepo

The "foobar" variable is not defined:
  $ mkpkg foo <<EOF
  > depexts: [[ "unzip" ] {foobar}]
  > EOF

Make a project that uses the foo library. Locking should succeed and not include
the "unzip" package since the filter variable is undefined.

  $ solve_project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name bar)
  >  (depends foo))
  > EOF
  Solution for dune.lock:
  - foo.0.0.1

  $ cat ${default_lock_dir}/foo.0.0.1.pkg
  (version 0.0.1)
  
  (depexts
   ((unzip) %{pkg-self:foobar}))
