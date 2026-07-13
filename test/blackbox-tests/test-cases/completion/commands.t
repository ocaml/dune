Completion scripts fall back to the shell's default filename completion when
Dune has no semantic completions to offer.

Bash registers the cmdliner completion function with the shell's fallback
completion options.

  $ dune completion bash | grep '^complete '
  complete -o bashdefault -o default -F _dune_cmdliner dune

CR-someday Alizter: test the zsh and PowerShell scripts by running their
completion functions too. For now, check that they contain the fallback
wrappers.

  $ dune completion zsh | grep '^function _dune_cmdliner'
  function _dune_cmdliner_cmdliner {
  function _dune_cmdliner {

  $ dune completion zsh | grep -E 'compstate\[nmatches\]|_default'
    local nmatches="${compstate[nmatches]:-0}"
    if [[ "${compstate[nmatches]:-0}" == "$nmatches" ]]; then
      _default

  $ dune completion powershell | grep '^\$Global:_dune_cmdliner'
  $Global:_dune_cmdliner_cmdliner = {
  $Global:_dune_cmdliner = {

  $ dune completion powershell | grep -A2 'CompletionCompleters.*CompleteFilename(' | tail -n3
      return [Management.Automation.CompletionCompleters]::CompleteFilename(
        $wordToComplete
      )
