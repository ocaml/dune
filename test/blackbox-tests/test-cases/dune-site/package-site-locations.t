A package dependency should expose its site data to the action (#17031).

  $ write_package_site_test_project
  $ cat >> dune <<'EOF'
  > (rule
  >  (alias runtest)
  >  (deps (package foo))
  >  (action (run main foo.txt)))
  > EOF
  $ dune runtest > output 2>&1
  [1]
  $ grep '^foo\.txt:' output
  foo.txt: missing
