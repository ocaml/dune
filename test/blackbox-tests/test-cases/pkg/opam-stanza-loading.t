Parent-declared opaque directories have data-only semantics before traversal.
The declaration requires the unreleased extension.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name geometry) (allow_empty))
  > (package (name nested) (allow_empty))
  > (package (name literal) (allow_empty))
  > EOF
  $ mkdir -p vendor/child nested/vendor 'data[0]' data0 ignored sibling
  $ for dir in vendor vendor/child nested/vendor 'data[0]' ignored; do
  >   echo '(' > "$dir/dune"
  >   echo '(' > "$dir/dune-project"
  > done
  $ echo '(rule (target result) (action (write-file %{target} sibling)))' > sibling/dune
  $ echo '(rule (target result) (action (write-file %{target} literal-neighbor)))' > data0/dune
  $ cat > packages.inc <<'EOF'
  > (opam (package geometry) (dir vendor) (version 1.0))
  > (opam (package literal) (dir "data[0]") (version 1.0))
  > EOF
  $ cat > dune <<'EOF'
  > (include packages.inc)
  > (data_only_dirs ignored)
  > (subdir nested
  >  (opam (package nested) (dir vendor) (version 1.0))
  >  (rule (target native) (action (write-file %{target} nested))))
  > (rule (target parent) (action (write-file %{target} parent)))
  > EOF
  $ dune build parent 2>error
  [1]
  $ grep "^Error:" error | sort -u
  Error: 'opam' is available only when unreleased is enabled in the
  $ echo '(using unreleased 0.1)' >> dune-project
  $ dune build parent sibling/result nested/native data0/result
  $ cat _build/default/parent _build/default/sibling/result \
  >   _build/default/nested/native _build/default/data0/result
  parentsiblingnestedliteral-neighbor

Changing the selected directory restores native loading in the old child.

  $ rm vendor/dune-project
  $ rm -rf vendor/child
  $ echo '(rule (target restored) (action (write-file %{target} restored)))' > vendor/dune
  $ mkdir alternate
  $ echo '(' > alternate/dune
  $ sed 's/(dir vendor)/(dir alternate)/' packages.inc > packages.new
  $ mv packages.new packages.inc
  $ dune build vendor/restored
  $ cat _build/default/vendor/restored
  restored

Two declarations cannot claim the same directory, including across includes.

  $ echo '(opam (package literal) (dir alternate) (version 2.0))' >> dune
  $ dune build parent 2>error
  [1]
  $ grep '^Error:' error
  Error: Directory "alternate" is already used by another opam stanza.

The old self-marking form is rejected. The directory must be one literal child.

  $ echo '(opam (package geometry) (version 1.0))' > dune
  $ dune build 2>error
  [1]
  $ grep '^Error:' error
  Error: Field "dir" is missing
  $ for dir in '' . .. /absolute a/b ./a; do
  >   printf '(opam (package geometry) (dir "%s") (version 1.0))\n' "$dir" > dune
  >   dune build 2>error
  >   grep '^Error:' error
  > done
  Error: The opam directory must be an immediate subdirectory name.
  Error: The opam directory must be an immediate subdirectory name.
  Error: The opam directory must be an immediate subdirectory name.
  Error: The opam directory must be an immediate subdirectory name.
  Error: The opam directory must be an immediate subdirectory name.
  Error: The opam directory must be an immediate subdirectory name.
  $ echo '(opam (package geometry) (dir %{project_root}) (version 1.0))' > dune
  $ dune build 2>error
  [1]
  $ grep '^Error:' error
  Error: Atom or quoted string expected
