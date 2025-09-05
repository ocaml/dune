Test detection of common typos in package dependencies

  $ cat > dune-project << EOF
  > (lang dune 3.11)
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends
  >   (bar (= version))  ; Should detect typo and suggest :version
  >   (baz :with_test)   ; Should detect typo and suggest :with-test
  >   qux))
  > EOF


  $ dune build
  File "dune-project", line 6, characters 2-19:
  6 |   (bar (= version))  ; Should detect typo and suggest :version
        ^^^^^^^^^^^^^^^^^
  Warning: Possible typo in constraint for dependency "bar": '(= version)'
  might be a mistake.
  Hint: Did you mean to use the `:version` variable instead? Example: (depends
  (bar (= :version)))
  File "dune-project", line 7, characters 2-18:
  7 |   (baz :with_test)   ; Should detect typo and suggest :with-test
        ^^^^^^^^^^^^^^^^
  Warning: Possible typo in constraint for dependency "baz": ':with_test' might
  be a mistake.
  Hint: Did you mean to use ':with-test' instead? Example: (depends (baz
  :with-test))
