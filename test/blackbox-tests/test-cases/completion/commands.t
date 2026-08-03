Completion scripts fall back to the shell's default filename completion when
Dune has no semantic completions to offer.

Bash registers the cmdliner completion function with the shell's fallback
completion options.

  $ dune completion bash | grep '^complete '
  complete -o bashdefault -o default -F _dune_cmdliner dune

Check that the zsh and PowerShell scripts contain the fallback wrappers. The
shell-specific tests also exercise these wrappers in their respective shells.

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

Fish registers the completion function without file completion and falls back
to it explicitly when there are no semantic completions:

  $ dune completion fish | grep '^complete '
  complete -c dune -f -k -a "(_dune_cmdliner)"

  $ dune completion fish | grep -A1 'if test $found -eq 0'
    if test $found -eq 0
      __fish_complete_path $current
