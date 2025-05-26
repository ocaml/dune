Test detection and disabling of common typos in package dependency constraints

Setup a simple project with dependency typos
  $ cat > dune-project <<EOF
  > (lang dune 3.11)
  > (package
  >  (name test-pkg)
  >  (depends (bar (= version))))
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (public_name test-pkg)
  >  (name main))
  > EOF

  $ cat > main.ml <<EOF
  > let () = print_endline "Hello"
  > EOF

Test that typo warnings are shown by default
  $ dune build @all 2>&1 | grep -A2 -B2 "typo\|Warning.*version" || echo "No warnings found"
  4 |  (depends (bar (= version))))
                ^^^^^^^^^^^^^^^^^
  Warning: Possible typo in constraint for dependency "bar": '(= version)'
  might be a mistake.
  Hint: Did you mean to use the `:version` variable instead? Example: (depends

Test that typo warnings can be disabled
  $ DUNE_CONFIG__TYPO_WARNINGS=disabled dune build @all 2>&1 | grep -A2 -B2 "typo\|Warning.*version" || echo "No typo warnings found"
  No typo warnings found

Test the :with_test typo detection
  $ cat > dune-project <<EOF
  > (lang dune 3.11)
  > (package
  >  (name test-pkg)
  >  (depends (baz :with_test)))
  > EOF

  $ dune build @all 2>&1 | grep -A2 -B2 "typo\|Warning.*with_test" || echo "No warnings found"
  4 |  (depends (baz :with_test)))
                ^^^^^^^^^^^^^^^^
  Warning: Possible typo in constraint for dependency "baz": ':with_test' might
  be a mistake.
  Hint: Did you mean to use ':with-test' instead? Example: (depends (baz

Test that :with_test warnings can also be disabled  
  $ DUNE_CONFIG__TYPO_WARNINGS=disabled dune build @all 2>&1 | grep -A2 -B2 "typo\|Warning.*with_test" || echo "No typo warnings found"
  No typo warnings found
