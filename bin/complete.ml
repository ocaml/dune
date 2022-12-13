open Import
open Stdune

let all = Fdecl.create Dyn.opaque

module Script = struct
  let script =
    {|
_dune () {
    COMPREPLY=( $(dune complete command --position $COMP_CWORD -- ${COMP_WORDS[*]}) )
}

complete -F _dune dune
|}

  let term =
    let+ () = Term.const () in
    print_endline script

  let info = Cmd.info ~doc:"Output a bash completion script for dune." "script"

  let command = Cmd.v info term
end

module Command = struct
  let split_at n l =
    let rec go n l acc =
      match l with
      | h :: t when n > 0 -> go (n - 1) t (h :: acc)
      | _ -> (acc, l)
    in
    let rev_head, tail = go n l [] in
    (List.rev rev_head, tail)

  let compute_prefix cword cmdline =
    let first, next = split_at cword cmdline in
    (first, List.hd_opt next)

  let match_prefix word_at_completion_point w =
    match word_at_completion_point with
    | None -> true
    | Some prefix -> String.is_prefix ~prefix w

  let complete_using_cline cmd args =
    let cmd_args = cmd |> Cmdliner_cmd.get_info |> Cmdliner_info.Cmd.args in
    match Cmdliner_cline.create cmd_args args with
    | Ok cline -> Some (Cmdliner_cline.complete cline args)
    | Error _ -> None

  (** TODO: this does not handle ~rev args *)
  let arg_fits k i =
    let start = Cmdliner_info.Arg.pos_start k in
    let len = Cmdliner_info.Arg.pos_len k in
    i >= start
    &&
    match len with
    | None -> true
    | Some l -> i < start + l

  let split_once s c =
    match String.index s c with
    | Some i -> Some (String.split_n s i)
    | None -> None

  (** Complete [--arg=value] *)
  let complete_equal all_args arg_name value_prefix =
    let cmd_opt =
      List.find all_args ~f:(fun arg ->
          List.mem
            (Cmdliner_info.Arg.opt_names arg)
            arg_name ~equal:String.equal)
    in
    match cmd_opt with
    | None -> []
    | Some arg ->
      Cmdliner_info.Arg.complete arg (Some value_prefix)
      |> List.map ~f:(fun c -> Printf.sprintf "%s=%s" arg_name c)

  let complete_args cmd ~pos_only ~index word =
    let all_args =
      cmd |> Cmdliner_cmd.get_info |> Cmdliner_info.Cmd.args
      |> Cmdliner_info.Arg.Set.elements
    in
    let opt_args, pos_args =
      all_args |> List.partition ~f:Cmdliner_info.Arg.is_opt
    in
    let eq_syntax =
      match word with
      | None -> None
      | Some w -> split_once w '='
    in
    match eq_syntax with
    | Some (arg_name, value_prefix) ->
      complete_equal all_args arg_name value_prefix
    | None ->
      (if pos_only then []
      else List.concat_map ~f:Cmdliner_info.Arg.opt_names opt_args)
      @ List.concat_map pos_args ~f:(fun arg ->
            let pos_kind = Cmdliner_info.Arg.pos_kind arg in
            if arg_fits pos_kind index then Cmdliner_info.Arg.complete arg word
            else [])

  let rec find_cmd cmdline cmds =
    match cmdline with
    | [] -> Error (List.map cmds ~f:Cmdliner_cmd.name)
    | first :: other_args -> (
      match
        List.find_opt cmds ~f:(fun cmd ->
            String.equal first (Cmdliner_cmd.name cmd))
      with
      | None -> Error []
      | Some cmd -> (
        match cmd with
        | Cmd _ -> Ok (cmd, other_args)
        | Group (_, (_, cmds)) -> find_cmd other_args cmds))

  let completions_at cmds position cmdline =
    let args, word_at_completion_point = compute_prefix position cmdline in
    let completing = Option.is_some word_at_completion_point in
    let completions =
      match find_cmd (List.tl args) cmds with
      | Error words -> words
      | Ok (cmd, other_args) -> (
        match complete_using_cline cmd other_args with
        | None -> []
        | Some (Arg_value { arg; optional; index }) -> (
          match (optional, completing) with
          | false, _ | true, false ->
            Cmdliner_info.Arg.complete arg word_at_completion_point
          | true, true ->
            complete_args cmd ~pos_only:false ~index word_at_completion_point)
        | Some (Flag_or_pos_arg { index; pos_only }) ->
          complete_args cmd
            ~pos_only:(pos_only || not completing)
            ~index word_at_completion_point)
    in
    List.filter completions ~f:(match_prefix word_at_completion_point)

  let term =
    let+ cmdline =
      let open Arg in
      value & pos_all string [] & info ~doc:"The command line to complete" []
    and+ position =
      let open Arg in
      required
      & opt (some int) None
      & info
          ~doc:
            "The 0-indexed position at which to complete the command-line. For \
             example, when typing $(b,dune bui<tab>), the position is 1 and \
             when typing $(b,dune build <tab>) it is 2."
          [ "position" ]
    in
    let cmds = Fdecl.get all |> List.map ~f:Cmd.inspect in
    completions_at cmds position cmdline
    |> String.Set.of_list
    |> String.Set.iter ~f:print_endline

  let info =
    Cmd.info ~doc:"Output possible completions of a partial dune command line."
      "command"

  let command = Cmd.v info term
end

module Test = struct
  module Several_pos = struct
    let info = Cmd.info "several-pos"

    let term =
      let c completions =
        Arg.conv
          ~complete:(fun _ -> completions)
          ((fun _ -> Ok ()), fun _ () -> ())
      in
      let+ () = Arg.(value & pos 0 (c [ "a1"; "a2" ]) () & info [])
      and+ () = Arg.(value & pos 1 (c [ "b1"; "b2" ]) () & info []) in
      ()

    let command = Cmd.v info term
  end

  module List = struct
    let info = Cmd.info "list"

    let term =
      let c =
        Arg.conv
          ~complete:(fun _ -> [ "aa"; "bb" ])
          ((fun _ -> Ok ()), fun _ () -> ())
      in
      let+ (_ : unit list) = Arg.(value & pos 0 (list c) [] & info []) in
      ()

    let command = Cmd.v info term
  end

  let info = Cmd.info "test"

  let group = Cmd.group info [ Several_pos.command; List.command ]
end

let info = Cmd.info "complete"

let command = Cmd.group info [ Script.command; Command.command; Test.group ]
