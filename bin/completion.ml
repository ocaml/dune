open Import

module type Shell = sig
  val name : string
  val aliases : string list
  val description : Manpage.block list
  val completion_script : fun_name:string -> string
end

module Bash : Shell = struct
  let name = "bash"
  let aliases = []

  let description =
    [ `I
        ( "$(b,bash)"
        , "Print out a bash completion script. It should then be written to a file that \
           will be sourced, for example in \
           ~/.local/share/bash-completion/completions/dune. Alternatively, it can be \
           sourced directly by adding this line to your .bashrc:" )
    ; `Pre
        {|
      eval "\$(dune completion bash)"|}
    ]
  ;;

  let completion_script ~fun_name =
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
  ;;
end

module Zsh : Shell = struct
  let name = "zsh"
  let aliases = []

  let description =
    [ `I
        ( "$(b,zsh)"
        , "Print out a zsh completion script. It should then be written to a file that \
           will be sourced, for example in ~/.local/share/zsh/site-functions/_dune. \
           Alternatively, it can be sourced directly by adding this line to your .zshrc:"
        )
    ; `Pre
        {|
          eval "\$(dune completion zsh)"|}
    ]
  ;;

  let completion_script ~fun_name =
    let fun_def = Cmdliner_data.zsh_generic_completion fun_name in
    sprintf
      {|#compdef %s
%s
%s
|}
      "dune"
      fun_def
      fun_name
  ;;
end

module Powershell : Shell = struct
  let name = "pwsh"
  let aliases = [ "powershell" ]

  let description =
    [ `I
        ( "$(b,pwsh) or $(b,powershell)"
        , "Print out a powershell completion script. It should then be written to a file \
           that will be sourced, for example in \\$PROFILE.CurrentUserCurrentHost. \
           Alternatively, it can be sourced directly by adding this line to your profile \
           script:" )
    ; `Pre
        {|
      dune completion pwsh | Out-String | Invoke-Expression|}
    ]
  ;;

  let completion_script ~fun_name =
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
end

let shells : (module Shell) list = [ (module Bash); (module Zsh); (module Powershell) ]

let all =
  shells
  |> List.concat_map ~f:(fun (module S : Shell) ->
    List.map (S.name :: S.aliases) ~f:(fun name -> name, (module S : Shell)))
;;

let all_with_aliases =
  shells
  |> List.map ~f:(fun (module M : Shell) -> M.name :: M.aliases, (module M : Shell))
;;

let term =
  let info_ = Arg.info ~doc:None ~docv:"SHELL" [] in
  let+ (module S) = Arg.(required & pos 0 (some (enum all)) None & info_) in
  print_string (S.completion_script ~fun_name:"_dune_cmdliner")
;;

let command =
  let info =
    let doc = "Generate shell completion scripts." in
    let man =
      [ `S "DESCRIPTION"
      ; `P
          "Generate a completion script in the specified shell for the dune binary. This \
           will enable tab-completion of dune commands, subcommands, and flags."
      ; `S "SHELL"
      ; `P "The shell for which you want dune completion."
      ]
      @ List.concat_map all_with_aliases ~f:(fun (_name, (module S : Shell)) ->
        S.description)
    in
    Cmd.info "completion" ~doc ~man
  in
  Cmd.v info term
;;
