This test tries to load the rules in a directory that is a target of another
rule.

  $ cat > dune-project <<EOF
  > (lang dune 3.4)
  > (using directory-targets 0.1)
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (deps (sandbox always))
  >  (targets (dir output))
  >  (action (bash "echo creating output dir && mkdir -p output/a && touch output/a/b")))
  > EOF

  $ loadedDirs() {
  > jq -c '.[] | select(.name == "load-dir") | .args'
  > }

  $ build() {
  > dune build $@
  > dune trace cat | loadedDirs
  > }

  $ export DUNE_TRACE=debug

  $ build output/
  creating output dir
  {"dir":"_build/default"}
  {"dir":"_build/default/.dune"}
  {"dir":"_build"}
  $ find _build/default/output
  _build/default/output
  _build/default/output/a
  _build/default/output/a/b

We are loading the rules in output/a and while making sure that we don't delete
and re-create output/b. The following should not re-run the rule that recreates
output/

  $ build output/a/b
  {"dir":"_build/default/output/a"}
  {"dir":"_build/default"}
  {"dir":"_build/default/.dune"}
  {"dir":"_build"}
  $ find _build/default/output
  _build/default/output
  _build/default/output/a
  _build/default/output/a/b

Now we try loading the rules in output/a and make sure that nothing is deleted:

  $ dune rules output/a/
  ((deps ())
   (targets ((files ()) (directories (_build/default/output))))
   (context default)
   (action
    (chdir
     _build/default
     (bash "echo creating output dir && mkdir -p output/a && touch output/a/b"))))

  $ dune trace cat | loadedDirs
  {"dir":"_build/default/output"}
  {"dir":"_build/default"}
  {"dir":"_build/default/.dune"}
  {"dir":"_build"}

  $ find _build/default/output
  _build/default/output
  _build/default/output/a
  _build/default/output/a/b
