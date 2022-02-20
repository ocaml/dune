Tests for dynamic dependencies computed from the `%{read:...}` family of macros

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF

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

  $ dune build ./output --display=short
            sh deps.d
            sh depA
            sh depB
            sh output
  contentsA
  contentsB
            sh output

  $ cat ./_build/default/output
  depA depB
