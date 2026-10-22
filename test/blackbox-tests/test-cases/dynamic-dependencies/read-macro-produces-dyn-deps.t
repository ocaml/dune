Tests for dynamic dependencies computed from the `%{read:...}` family of macros

  $ make_dune_project 3.0

Define rules have dynamic file dependencies

  $ cat > dune <<EOF
  > (rule
  >  (target deps.d)
  >  (action
  >   (system "echo 'depA\ndepB' > %{target}")))
  > 
  > (rule
  >  (target depA)
  >  (action
  >   (system "echo contentsA > %{target}")))
  > 
  > (rule
  >  (target depB)
  >  (action
  >   (system "echo contentsB > %{target}")))
  > EOF

Now we define a rule that reads `deps.d` to figure out what to build.

  $ cat >> dune <<EOF
  > (rule
  >  (target output)
  >  (deps %{read-lines:./deps.d})
  >  (action
  >   (progn
  >    (system "cat %{deps}")
  >    (system "echo %{deps} > %{target}"))))
  > EOF

Building `./output` should now produce a file with contents "depA depB"

  $ dune build ./output
  contentsA
  contentsB

  $ cat ./_build/default/output
  ./depA ./depB

The read-strings variant decodes OCaml escapes before treating each line as a
single dependency. A space in a filename must not split it into two paths.

  $ cat > filenames <<'EOF'
  > space\032name
  > EOF
  $ echo contents > 'space name'
  $ cat >> dune <<'EOF'
  > (rule
  >  (target escaped)
  >  (deps %{read-strings:filenames})
  >  (action (with-stdout-to %{target} (cat %{deps}))))
  > EOF
  $ dune build escaped
  $ cat _build/default/escaped
  contents

Changes to the decoded dependency invalidate the action.

  $ echo changed > 'space name'
  $ dune build escaped
  $ cat _build/default/escaped
  changed

Invalid escapes are attributed to the file being read, not to the rule.

  $ printf '%s\n' '\q' > filenames
  $ dune build escaped
  File "_build/default/filenames", line 1, characters 0-0:
  Error: This file must be a list of lines escaped using OCaml's conventions
  [1]
