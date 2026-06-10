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
complete -F %s dune
|}
      fun_def
      fun_name
      fun_name
      fun_name
  | Zsh ->
    let fun_def = Cmdliner_data.zsh_generic_completion fun_name in
    sprintf
      {|#compdef dune
%s
%s
|}
      fun_def
      fun_name
  | Pwsh ->
    let fun_def = Cmdliner_data.pwsh_generic_completion fun_name in
    sprintf
      {|
%s

Register-ArgumentCompleter -Native -CommandName dune -ScriptBlock %s
|}
      fun_def
      fun_name
;;

let term shell =
  let+ () = Term.const () in
  User_message.print (User_message.make [ Pp.text (completion_script shell) ])
;;

let bash =
  let info =
    let doc = "TODO doc1" in
    let man = [ `P "TODO description1" ] in
    Cmd.info "bash" ~doc ~man
  in
  Cmd.v info (term Bash)
;;

let zsh =
  let info =
    let doc = "TODO doc2" in
    let man = [ `P "TODO description2" ] in
    Cmd.info "zsh" ~doc ~man
  in
  Cmd.v info (term Zsh)
;;

let pwsh =
  let info =
    let doc = "TODO doc3" in
    let man = [ `P "TODO description3" ] in
    Cmd.info "pwsh" ~doc ~man
  in
  Cmd.v info (term Pwsh)
;;

let command =
  let info =
    let doc = "TODO doc" in
    let man = [ `S "DESCRIPTION"; `P "TODO description" ] in
    Cmd.info "completion" ~doc ~man
  in
  Cmd.group info [ bash; zsh; pwsh ]
;;
