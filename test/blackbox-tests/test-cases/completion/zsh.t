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

Semantic directory candidates suppress Zsh's default trailing-space suffix.
Use a synthetic completion response so this behavior is covered independently
of any command-specific semantic completer.

  $ cat > semantic-completion <<'EOF'
  > #!/bin/sh
  > printf '%s\n' 1 item tests/ item-end
  > EOF
  $ chmod +x semantic-completion
  $ zsh -f <<EOF
  > typeset -A compstate
  > compstate[nmatches]=0
  > _describe() {
  >   local completion_name=\$3
  >   local -a described=( "\${(@P)completion_name}" )
  >   local completion
  >   for completion in "\${described[@]}"; do
  >     print -r -- "\${completion%%:*}"
  >   done
  >   if (( \${argv[(I)-S]} )); then
  >     print -r -- "empty suffix"
  >   else
  >     print -r -- "trailing space"
  >   fi
  >   (( compstate[nmatches] += \${#described} ))
  > }
  > _default() {}
  > words=("$PWD/semantic-completion" tes)
  > CURRENT=2
  > source <(dune completion zsh)
  > EOF
  tests/
  empty suffix
