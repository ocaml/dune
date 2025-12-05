Solving with an unknown variable on depexts:

  $ . ../helpers.sh
  $ mkrepo
  $ add_mock_repo_if_needed

The "foobar" variable is not defined:
  $ mkpkg foo <<EOF
  > depexts: [[ "unzip" ] {foobar}]
  > EOF

Make a project that uses the foo library:
  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name bar)
  >  (depends foo))
  > EOF

Locking should succeed and not include the "unzip" package
  $ dune pkg lock 2>&1 | head -n 1
  Solution for dune.lock

  $ cat ${default_lock_dir}/foo.0.0.1.pkg
  (version 0.0.1)
  
  (depexts
   ((unzip) %{pkg-self:foobar}))
