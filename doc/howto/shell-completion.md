How to Set Up Shell Command Completion
======================================

Shell command completion refers to a common feature in various shells:
hitting \<TAB\> after a partially-typed command will print matching suggestions.
This can apply to command names, flags, arguments...

Dune offers completion for commands, subcommands, and flags, for Bash, Z shell, and PowerShell.
The various shells and systems require specific configuration setups,
and this document aims to help users get completion running.

Bash
----

Command completion for bash might require [bash-completion](https://github.com/scop/bash-completion).
You will then need to create the bash completion script (by running `dune completion bash`)
and write that script to an appropriate place: by default that should be
`~/.local/share/bash-completion/completions/dune` for a user-local configuration,
or `/usr/local/share/bash-completion/completions/dune` for a system-wide installation.
If you use `opam` switches and wish to have switch-local completion,
add the script to `<switch prefix>/share/bash-completion/completions/dune`.

A different option would be to add to your `.bashrc` the following line: `eval "$(dune completion bash)"`.

You might need to restart your shell to see the effects.

Zsh
---

Command completion for zsh might require [zsh-completions](https://github.com/zsh-users/zsh-completions).
You will then need to create the zsh completion script (by running `dune completion zsh`)
and write that script to a place that will get autoloaded: directories present
in your `$fpath` (by default it should be `~/.local/share/zsh/site-functions`).
If you wish to create a dedicated completion directory and add it to your `$fpath`,
be sure to add the following in your `.zshrc` but **before** any call to `compinit`:
`fpath+=<your dir>`.

You might need to restart your shell to see the effects.

PowerShell
----------

Command completion for PowerShell should work out of the box:
simply run `dune completion powershell >> $PROFILE.CurrentUserCurrentHost`.
Alternatively, add the following to your profile script:
`dune completion powershell | Out-String | Invoke-Expression`.

You might need to restart your shell to see the effects.
