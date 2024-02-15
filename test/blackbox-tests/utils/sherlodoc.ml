(** Test double for sherlodoc: it understands `js` and `index` commands, and
    creates their output files. *)

open Stdune

let arg_db = ref ""
let args_favored = ref []

let args_index =
  [ "--format", Arg.String ignore, ""
  ; "--favoured-prefixes", Arg.String ignore, ""
  ; ( "--favoured"
    , Arg.String (fun fav_odocl -> args_favored := fav_odocl :: !args_favored)
    , "" )
  ; "--db", Arg.Set_string arg_db, ""
  ]
;;

let parse_index_args args =
  let inputs = ref [] in
  Arg.parse_argv
    (Array.of_list args)
    args_index
    (fun input -> inputs := input :: !inputs)
    "";
  !inputs, !arg_db
;;

let () =
  match Array.to_list Sys.argv with
  | [] -> assert false
  | [ _; "js"; output ] ->
    Out_channel.with_open_bin output (fun oc ->
      Out_channel.output_string oc "/* Output of sherlodoc js */\n")
  | _ :: "index" :: index_args ->
    let deps, target = parse_index_args index_args in
    Out_channel.with_open_bin target (fun oc ->
      Out_channel.output_string oc "/* Sherlodoc DB for: */\n";
      List.iter deps ~f:(fun dep -> Printf.fprintf oc "/*   - %s */\n" dep);
      List.iter !args_favored ~f:(fun dep ->
        Printf.fprintf oc "/*   - --favored %s */\n" dep))
  | _ :: args ->
    Printf.ksprintf failwith "sherlodoc(fake): %s" (String.concat ~sep:"," args)
;;
