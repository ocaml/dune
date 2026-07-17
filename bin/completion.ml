open Import

module type Shell = sig
  val name : string
  val description : Manpage.block list
  val completion_script : fun_name:string -> string
end

module Bash : Shell = struct
  let name = "bash"

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
complete -o bashdefault -o default -F %s %s
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

  let description =
    [ `I
        ( "$(b,zsh)"
        , "Print out a zsh completion script. It should then be written to a file named \
           _dune in a directory that will be autoloaded, for example in \
           ~/.local/share/zsh/site-functions. Make sure that that directory is in your \
           \\$fpath by adding this line to your .zshrc before `compinit`:" )
    ; `Pre
        {|
      fpath+=~/.local/share/zsh/site-functions|}
    ]
  ;;

  let completion_script ~fun_name =
    let inner_fun_name = fun_name ^ "_cmdliner" in
    let fun_def = Cmdliner_data.zsh_generic_completion inner_fun_name in
    sprintf
      {|#compdef %s
%s
function %s {
  local nmatches="${compstate[nmatches]:-0}"
  %s
  local ret=$?
  if [[ "${compstate[nmatches]:-0}" == "$nmatches" ]]; then
    _default
  fi
  return $ret
}
%s
|}
      "dune"
      fun_def
      fun_name
      inner_fun_name
      fun_name
  ;;
end

module Powershell : Shell = struct
  let name = "powershell"

  let description =
    [ `I
        ( "$(b,powershell)"
        , "Print out a powershell completion script. It should then be written to a file \
           that will be sourced, for example in \\$PROFILE.CurrentUserCurrentHost. \
           Alternatively, it can be sourced directly by adding this line to your profile \
           script:" )
    ; `Pre
        {|
      dune completion powershell | Out-String | Invoke-Expression|}
    ]
  ;;

  let completion_script ~fun_name =
    let inner_fun_name = fun_name ^ "_cmdliner" in
    let fun_def = Cmdliner_data.pwsh_generic_completion inner_fun_name in
    sprintf
      {|
%s

$Global:%s = {
  param(
    $wordToComplete,
    $commandAst,
    $cursorPosition
  )

  $CompletionResults = & $%s $wordToComplete $commandAst $cursorPosition
  if ($null -ne $CompletionResults -and @($CompletionResults).Count -gt 0) {
    return $CompletionResults
  }
  else {
    return [Management.Automation.CompletionCompleters]::CompleteFilename(
      $wordToComplete
    )
  }
}

Register-ArgumentCompleter -Native -CommandName %s -ScriptBlock $%s
|}
      fun_def
      fun_name
      inner_fun_name
      "dune"
      fun_name
  ;;
end

module Fish : Shell = struct
  let name = "fish"

  let description =
    [ `I
        ( "$(b,fish)"
        , "Print out a fish completion script. It should then be written to a file named \
           dune.fish in a directory that fish autoloads completions from, for example \
           ~/.config/fish/completions:" )
    ; `Pre
        {|
      dune completion fish > ~/.config/fish/completions/dune.fish|}
    ]
  ;;

  (* cmdliner does not provide a generic completion script for fish, so it is
     maintained here. It speaks the same completion protocol as the scripts in
     [Cmdliner_data]. *)
  let completion_script ~fun_name =
    sprintf
      {|function %s
  set -l tokens (commandline -opc)
  set -l current (commandline -ct)
  type -q $tokens[1]; or return
  set -l response ($tokens[1] --__complete $tokens[2..-1] --__complete=$current 2>/dev/null)
  if not set -q response[1]
    __fish_complete_path $current
    return
  end
  if test "$response[1]" != 1
    echo "Unsupported cmdliner completion protocol: $response[1]" >&2
    return 1
  end
  set -l found 0
  set -l group ""
  set -l n (count $response)
  set -l i 2
  while test $i -le $n
    set -l type $response[$i]
    set i (math $i + 1)
    switch $type
      case group
        set group $response[$i]
        set i (math $i + 1)
      case item
        set -l item $response[$i]
        set i (math $i + 1)
        set -l doc
        while test $i -le $n; and test "$response[$i]" != item-end
          set -a doc $response[$i]
          set i (math $i + 1)
        end
        set i (math $i + 1)
        set -l item_doc
        if set -q doc[1]
          set item_doc (string join -- " " $doc | string replace -ra '\e\[[0-9;]*m' '')
        end
        # Handle glued forms, the completion item is the full token
        if test "$group" = Values
          if string match -q -- '--*' $current
            set -l opt (string split -m 1 -- = $current)[1]
            set item $opt=$item
          else if string match -q -- '-*' $current
            set item (string sub -l 2 -- $current)$item
          end
        end
        string join -- \t $item $item_doc
        set found (math $found + 1)
      case files dirs
        # trim option prefix in cases like --file=<TAB> or -f<TAB>
        set -l prefix ""
        set -l pattern $current
        if string match -q -- '--*' $current
          set -l parts (string split -m 1 -- = $current)
          if set -q parts[2]
            set prefix $parts[1]=
            set pattern $parts[2]
          end
        else if string match -q -- '-*' $current
          set prefix (string sub -l 2 -- $current)
          set pattern (string sub -s 3 -- $current)
        end
        set -l paths
        if test $type = dirs
          set paths (__fish_complete_directories $pattern)
        else
          set paths (__fish_complete_path $pattern)
        end
        if set -q paths[1]
          string replace -r -- '^' $prefix $paths
          set found (math $found + (count $paths))
        end
      case message
        while test $i -le $n; and test "$response[$i]" != message-end
          set i (math $i + 1)
        end
        set i (math $i + 1)
      case restart
        # N.B. only emitted if there is a -- token
        set -l sep (contains -i -- -- $tokens)
        if test -n "$sep"
          set -l subline (string join -- " " (string escape -- $tokens[(math $sep + 1)..-1] $current))
          set -l results (complete -C"$subline")
          if set -q results[1]
            string join -- \n $results
            set found (math $found + (count $results))
          end
        end
    end
  end
  if test $found -eq 0
    __fish_complete_path $current
  end
end
complete -c %s -f -k -a "(%s)"
|}
      fun_name
      "dune"
      fun_name
  ;;
end

let shells : (module Shell) list =
  [ (module Bash); (module Zsh); (module Fish); (module Powershell) ]
;;

let all = shells |> List.map ~f:(fun (module S : Shell) -> S.name, (module S : Shell))

let term =
  let info_ =
    Arg.info
      ~doc:
        (Some
           (sprintf
              "The shell for which you want dune completion. Possible values are %s"
              (String.enumerate_or (all |> List.map ~f:fst))))
      ~docv:"SHELL"
      []
  in
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
      @ List.concat_map all ~f:(fun (_name, (module S : Shell)) -> S.description)
    in
    Cmd.info "completion" ~doc ~man
  in
  Cmd.v info term
;;
