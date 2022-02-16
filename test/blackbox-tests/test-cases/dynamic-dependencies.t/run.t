Tests for dynamic dependencies computed from the `%{read:...}` family of macros

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF

Define 2 rules and a file containing their paths

  $ cat > deps.d <<EOF
  > depA
  > depB
  > EOF

  $ cat > dune <<EOF
  > (rule
  >  (target depA)
  >  (action
  >   (bash "echo depA > %{target}")))
  > 
  > (rule
  >  (target depB)
  >  (action
  >   (bash "echo depB > %{target}")))
  > EOF

Now we define a rule that reads `deps.d` to figure out what to build.

  $ cat >> dune <<EOF
  > (rule
  >  (target output)
  >  (deps (include deps.d))
  >  (action
  >   (bash "echo %{deps} > %{target}")))
  > EOF

Building `./output` should now produce a file with contents "depA depB"

  $ dune build ./output --display=short
          bash depA
          bash depB
          bash output

Doesn't work in dune pre 3.0

  $ cat > dune-project <<EOF
  > (lang dune 2.9)
  > EOF

  $ dune build ./output --display=short
  File "dune", line 12, characters 7-23:
  12 |  (deps (include deps.d))
              ^^^^^^^^^^^^^^^^
  Error: 'include' is only available since version 3.0 of the dune language.
  Please update your dune-project file to have (lang dune 3.0).
  [1]

Works with aliases and other dependency specifications

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF

  $ cat > deps.d <<EOF
  > (alias depA)
  > (universe)
  > depB
  > another_dep
  > EOF

  $ cat > dune <<EOF
  > (rule
  >  (alias depA)
  >  (action
  >   (bash "echo building depA")))
  > 
  > (rule
  >  (target depB)
  >  (action
  >   (progn
  >     (run echo building depB)
  >     (bash "echo depB > %{target}"))))
  > (rule
  >  (target another_dep)
  >  (action
  >   (bash "echo building a_dep > %{target}")))
  > 
  > (rule
  >  (alias output)
  >  (deps (include deps.d))
  >  (action
  >   (run echo "dependencies %{deps}")))
  > EOF

  $ dune build @output --display=short
          bash alias depA
  building depA
          echo depB
  building depB
          bash another_dep
          bash depB
          echo alias output
  dependencies depB another_dep

