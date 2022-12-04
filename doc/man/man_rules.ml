open Stdune
(* open Dune_bin.Import *)

let man_rule (cmd : string list) =
  let action =
    String.concat ~sep:" "
    @@
    match cmd with
    | "dune" :: sc -> "%{dune.exe}" :: sc
    | _ ->
      Code_error.raise
        "Non-dune command detected. This is not supported yet, please add a \
         case here."
        []
  in
  let dash_seperated = String.concat ~sep:"-" cmd in
  sprintf
    {|

(rule
 (mode promote)
 (aliases default doc check)
 (action
  (with-stdout-to
   %s.1
   (run %s --help=plain))))
|}
    dash_seperated action

let rec all_commands_of_cmdliner rev_cmd_acc (cmd : unit Cmdliner.Cmd.t) =
  match Cmdliner.Cmd.get_subcommands cmd with
  | [] -> [ Cmdliner.Cmd.name cmd :: rev_cmd_acc ]
  | cmds ->
    List.rev_map cmds
      ~f:(all_commands_of_cmdliner (Cmdliner.Cmd.name cmd :: rev_cmd_acc))
    |> List.flatten

let main () =
  all_commands_of_cmdliner [] Dune_bin.Dune_cmd.cmd
  |> List.rev_map ~f:man_rule
  |> List.iter ~f:(Printf.printf "%s")

let () = main ()
