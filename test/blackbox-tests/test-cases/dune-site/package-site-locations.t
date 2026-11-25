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

An empty directory-level binding should not be appended as a fallback.

  $ cp dune rules
  $ echo '(env (_ (env-vars (DUNE_DIR_LOCATIONS ""))))' >> dune
  $ dune runtest > output 2>&1
  [1]
  $ grep -q 'Invalid value DUNE_DIR_LOCATIONS' output
  $ mv rules dune

Repeating a package through an include should not repeat its site path.

  $ echo '((package foo))' > deps.sexp
  $ cat >> dune <<'EOF'
  > (rule
  >  (alias repeated-dep)
  >  (deps (package foo) (include deps.sexp))
  >  (action (run main)))
  > EOF
  $ cat > main.ml <<'EOF'
  > let () =
  >   let paths = Sites.Sites.data in
  >   let unique = List.sort_uniq String.compare paths in
  >   Printf.printf "%d paths, %d unique\n" (List.length paths) (List.length unique)
  > EOF
  $ dune build @repeated-dep
  1 paths, 1 unique

A regular file at the exact site path does not populate the site as a directory.
It adds neither a scoped location nor an environment dependency. The legacy
staging location is still visible.

  $ echo '(package (name bar))' >> dune-project
  $ echo bar > bar.txt
  $ cat >> dune <<'EOF'
  > (install
  >  (package bar)
  >  (section share_root)
  >  (files (bar.txt as foo/data)))
  > (rule
  >  (target no-sites)
  >  (deps (package bar))
  >  (action (with-stdout-to %{target} (run ./main.exe))))
  > EOF
  $ dune build no-sites
  $ cat _build/default/no-sites
  1 paths, 1 unique
  $ dune rules --format=json _build/default/no-sites \
  >   | jq '[.[].deps[] | select(.Env == "DUNE_DIR_LOCATIONS")]'
  []
