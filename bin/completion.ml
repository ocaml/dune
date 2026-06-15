open Import

type shell =
  | Bash
  | Zsh
  | Pwsh

let completion_script shell =
  let fun_name = "_dune_cmdliner" in
  match shell with
  | Bash ->
    let fun_def = Cmdliner_data.bash_generic_completion fun_name in
    sprintf
      {|
%s
if ! declare -F %s > /dev/null; then
  _completion_loader %s
fi
complete -F %s %s
|}
      fun_def
      fun_name
      fun_name
      fun_name
      "dune"
  | Zsh ->
    let fun_def = Cmdliner_data.zsh_generic_completion fun_name in
    sprintf
      {|#compdef %s
%s
%s
|}
      "dune"
      fun_def
      fun_name
  | Pwsh ->
    let fun_def = Cmdliner_data.pwsh_generic_completion fun_name in
    sprintf
      {|
%s

Register-ArgumentCompleter -Native -CommandName %s -ScriptBlock $%s
|}
      fun_def
      "dune"
      fun_name
;;

let term shell =
  let+ () = Term.const () in
  print_string (completion_script shell)
;;

let bash =
  let info =
    let doc = "Generate a bash completion script for the dune binary." in
    let man =
      [ `P
          "Print out a bash completion script. It should then be written to a file that \
           will be sourced, for example in \
           ~/.local/share/bash-completion/completions/dune, or it can be sourced \
           directly by adding this line to your .bashrc:"
      ; `Pre
          {|
      eval "\$(dune completion bash)"|}
      ]
    in
    Cmd.info "bash" ~doc ~man
  in
  Cmd.v info (term Bash)
;;

let zsh =
  let info =
    let doc = "Generate a zsh completion script for the dune binary." in
    let man =
      [ `P
          "Print out a zsh completion script. It should then be written to a file that \
           will be sourced, for example in ~/.local/share/zsh/site-functions/_dune, or \
           it can be sourced directly by adding this line to your .zshrc:"
      ; `Pre
          {|
          eval "\$(dune completion zsh)"|}
      ]
    in
    Cmd.info "zsh" ~doc ~man
  in
  Cmd.v info (term Zsh)
;;

let pwsh =
  let info =
    let doc = "Generate a powershell completion script for the dune binary." in
    let man =
      [ `P
          "Print out a powershell completion script. It should then be written to a file \
           that will be sourced, for example in \\$PROFILE.CurrentUserCurrentHost, or it \
           can be sourced directly by adding this line to your profile script:"
      ; `Pre
          {|
      Invoke-Expression -Command \$(dune completion pwsh | Out-String)|}
      ]
    in
    Cmd.info "pwsh" ~doc ~man
  in
  Cmd.v info (term Pwsh)
;;

let command =
  let info =
    let doc = "Generate shell completion scripts." in
    let man =
      [ `S "DESCRIPTION"
      ; `P
          "Generate a completion script in the specified shell for the dune binary. This \
           script should be written to a file that will then get sourced."
      ]
    in
    Cmd.info "completion" ~doc ~man
  in
  Cmd.group info [ bash; zsh; pwsh ]
;;
