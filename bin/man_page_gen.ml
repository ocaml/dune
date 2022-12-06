open Stdune

let cmdliner_cmd = Dune_cmd.cmd

let write_man_page (cmd : string list) =
  let dash_seperated = String.concat ~sep:"-" cmd in
  let oc = open_out (dash_seperated ^ ".1") in
  let argv = cmd @ [ "--help=plain" ] |> Array.of_list in
  let (_ : (unit Cmdliner.Cmd.eval_ok, Cmdliner.Cmd.eval_error) result) =
    Cmdliner.Cmd.eval_value ~argv
      ~help:(Format.formatter_of_out_channel oc)
      cmdliner_cmd
  in
  close_out oc

let all_commands_of_cmdliner =
  let rec loop rev_cmd_acc cmd =
    match Cmdliner.Cmd.get_subcommands cmd with
    | [] -> [ Cmdliner.Cmd.name cmd :: rev_cmd_acc ]
    | cmds ->
      List.rev_map cmds ~f:(loop (Cmdliner.Cmd.name cmd :: rev_cmd_acc))
      |> List.flatten
  in
  loop [] cmdliner_cmd |> List.rev_map ~f:List.rev

let () = all_commands_of_cmdliner |> List.iter ~f:write_man_page
