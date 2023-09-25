Check that shell = %{bin:zsh} uses zsh

  $ echo '(lang dune 3.12)' > dune-project
  $ cat > dune <<EOF
  > (cram (shell %{bin:zsh}))
  > EOF

  $ cat > foo.t <<EOF
  >   $ echo "foo from foo.t"
  > 
  > EOF
  $ echo '  $ [ ! -z $ZSH_NAME ] && echo "shell = zsh" || echo "shell <> zsh"' >> foo.t

  $ dune runtest --auto-promote
  File "foo.t", line 1, characters 0-0:
  Error: Files _build/default/foo.t and _build/default/foo.t.corrected differ.
  Promoting _build/default/foo.t.corrected to foo.t.
  [1]

  $ dune runtest -f
  $ cat foo.t
    $ echo "foo from foo.t"
    foo from foo.t
  
    $ [ ! -z $ZSH_NAME ] && echo "shell = zsh" || echo "shell <> zsh"
    shell = zsh
