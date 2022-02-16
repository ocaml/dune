Tests for dynamic dependencies computed from the `%{read:...}` family of macros

  $ cat > dune-project <<EOF
  > (lang dune 3.1)
  > EOF

Define 2 rules and a file containing their paths

  $ cat > deps.d <<EOF
  > (depA depB)
  > EOF

  $ cat > dune <<EOF
  > (rule
  >  (target depA)
  >  (action
  >   (system "echo depA > %{target}")))
  > 
  > (rule
  >  (target depB)
  >  (action
  >   (system "echo depB > %{target}")))
  > EOF

Now we define a rule that reads `deps.d` to figure out what to build.

  $ cat >> dune <<EOF
  > (rule
  >  (target output)
  >  (deps (include deps.d))
  >  (action
  >   (system "echo %{deps} > %{target}")))
  > EOF

Building `./output` should now produce a file with contents "depA depB"

  $ dune build ./output --display=short
            sh depA
            sh depB
            sh output

Doesn't work in dune pre 3.0

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF

  $ dune build ./output --display=short
  File "dune", line 12, characters 7-23:
  12 |  (deps (include deps.d))
              ^^^^^^^^^^^^^^^^
  Error: 'include' is only available since version 3.1 of the dune language.
  Please update your dune-project file to have (lang dune 3.1).
  [1]

Works with aliases and other dependency specifications

  $ cat > dune-project <<EOF
  > (lang dune 3.1)
  > EOF

  $ cat > deps.d <<EOF
  > ((alias depA) (universe) depB another_dep)
  > EOF

  $ cat > dune <<EOF
  > (rule
  >  (alias depA)
  >  (action
  >   (system "echo building depA")))
  > 
  > (rule
  >  (target depB)
  >  (action
  >   (progn
  >     (run echo building depB)
  >     (system "echo depB > %{target}"))))
  > (rule
  >  (target another_dep)
  >  (action
  >   (system "echo building a_dep > %{target}")))
  > 
  > (rule
  >  (alias output)
  >  (deps (include deps.d))
  >  (action
  >   (no-infer (echo "dependencies %{deps}"))))
  > EOF

  $ dune build @output --display=short
            sh alias depA
  building depA
          echo depB
  building depB
            sh another_dep
            sh depB
  dependencies depB another_dep

Multiple `(include)` nesting

  $ cat >> dune <<EOF
  > (rule
  >  (target meta-deps.d)
  >  (action
  >   (system "echo '((include ./deps.d))' > %{target}")))
  > 
  > (rule
  >  (alias nested)
  >  (deps (include meta-deps.d))
  >  (action (no-infer (echo "metadeps: %{deps}"))))
  > EOF

  $ dune build @nested --display=short
            sh meta-deps.d
  metadeps: depB another_dep

