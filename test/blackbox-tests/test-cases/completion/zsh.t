Zsh completion scripts fall back to filename completion when Dune has no
semantic completions to offer.

This test sources the generated zsh completion script and exercises its
completion function. The display helpers are overridden so that the completion
results can be inspected outside the interactive line editor.

  $ mkdir completions
  $ touch completions/zsh.t completions/other-file
  $ cd completions
  $ dune completion zsh > _dune
  $ zsh -f <<'EOF'
  > typeset -A compstate
  > compstate[nmatches]=0
  > _describe() {
  >   local completion
  >   for completion in "${completions[@]}"; do
  >     print -r -- "${completion%%:*}"
  >   done
  >   (( compstate[nmatches] += ${#completions} ))
  > }
  > _default() {
  >   print -rl -- ${~words[CURRENT]}*
  > }
  > words=(dune runtes)
  > CURRENT=2
  > source ./_dune
  > words=(dune runtest zsh)
  > CURRENT=3
  > _dune_cmdliner
  > EOF
  runtest
  zsh.t
