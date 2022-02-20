Tests for dynamic dependencies computed from the `%{read:...}` family of macros

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF

Define rules have dynamic file dependencies

  $ cat > dune <<EOF
  > (rule
  >  (target deps.d)
  >  (action
  >   (bash "echo 'depA\ndepB' > %{target}")))
  > 
  > (rule
  >  (target depA)
  >  (action
  >   (bash "echo contentsA > %{target}")))
  > 
  > (rule
  >  (target depB)
  >  (action
  >   (bash "echo contentsB > %{target}")))
  > EOF

Now we define a rule that reads `deps.d` to figure out what to build.

  $ cat >> dune <<EOF
  > (rule
  >  (target output)
  >  (deps %{read-lines:./deps.d})
  >  (action
  >   (progn
  >    (bash "cat %{deps}")
  >    (bash "echo %{deps} > %{target}"))))
  > EOF

Building `./output` should now produce a file with contents "depA depB"

  $ dune build ./output --display=short
          bash deps.d
          bash depA
          bash depB
          bash output
  contentsA
  contentsB
          bash output

  $ cat ./_build/default/output
  depA depB
