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
  >  (deps %{read-lines:deps.d})
  >  (action
  >   (bash "echo %{deps} > %{target}")))
  > EOF

Building `./output` should now produce a file with contents "depA depB"

  $ dune build ./output --display=short
          bash depA
          bash depB
          bash output
