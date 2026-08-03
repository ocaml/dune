Bash completion scripts fall back to filename completion when Dune has no
semantic completions to offer.

This test sources the generated bash completion script, asks bash which
completion it registered for dune, and exercises that completion registration.

  $ mkdir completions
  $ touch completions/bash.t completions/other-file
  $ bash <<'EOF'
  > enable complete compgen
  > source <(dune completion bash)
  > _get_comp_words_by_ref() {
  >   local words_var="${@: -2:1}"
  >   local cword_var="${@: -1}"
  >   eval "$words_var=(\"\${COMP_WORDS[@]}\")"
  >   printf -v "$cword_var" "%s" "$COMP_CWORD"
  > }
  > cd completions
  > COMP_WORDS=(dune runtest bas)
  > COMP_CWORD=2
  > completion=( $(complete -p dune) )
  > completion[0]=compgen
  > unset "completion[${#completion[@]}-1]"
  > "${completion[@]}" -- bas 2>stderr
  > sed '/compgen: warning: -F option may not work as you expect/d' stderr
  > EOF
  bash.t
