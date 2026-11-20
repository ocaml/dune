Inline tests should see site data from both direct and included dependencies.

  $ write_package_site_test_project
  $ echo '(package (name addon))' >> dune-project
  $ echo addon > addon.txt
  $ echo '((package addon))' > deps.sexp
  $ cat >> dune <<'EOF'
  > (install (package addon) (section (site (foo data))) (files addon.txt))
  > (library
  >  (name backend)
  >  (modules ())
  >  (inline_tests.backend
  >   (generate_runner
  >    (echo "let () = exit (Sys.command" " \"main foo.txt addon.txt\")"))))
  > (library
  >  (name check)
  >  (modules ())
  >  (inline_tests
  >   (backend backend)
  >   (modes byte)
  >   (deps (package foo) (include deps.sexp))))
  > EOF
  $ dune runtest > output 2>&1
  [1]
  $ grep -E '^(foo|addon)\.txt:' output
  foo.txt: missing
  addon.txt: missing
