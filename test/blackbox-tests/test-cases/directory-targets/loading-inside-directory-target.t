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

  $ dune build --debug-load-dir output/
  Loading build directory _build/default
  Loading build directory _build/default/.dune
  Loading build directory _build
  creating output dir
  $ find _build/default/output
  _build/default/output
  _build/default/output/a
  _build/default/output/a/b

We are loading the rules in output/a and while making sure that we don't delete
and re-create output/b. The following should not re-run the rule that recreates
output/

  $ dune build --debug-load-dir output/a/b
  Loading build directory _build/default/output/a
  Loading build directory _build/default
  Loading build directory _build/default/.dune
  Loading build directory _build
  $ find _build/default/output
  _build/default/output
  _build/default/output/a
  _build/default/output/a/b

Now we try loading the rules in output/a and make sure that nothing is deleted:

  $ dune rules --debug-load-dir output/a/
  Loading build directory _build/default/output
  Loading build directory _build/default
  Loading build directory _build/default/.dune
  Loading build directory _build
  ((deps ())
   (targets ((files ()) (directories (default/output))))
   (context default)
   (action
    (chdir
     _build/default
     (bash "echo creating output dir && mkdir -p output/a && touch output/a/b"))))
  $ find _build/default/output
  _build/default/output
  _build/default/output/a
  _build/default/output/a/b
