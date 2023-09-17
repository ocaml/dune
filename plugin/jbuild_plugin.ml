let directive_table = Toploop.directive_table [@@ocaml.warning "-3"]

let () =
  Hashtbl.add directive_table "require" (Toploop.Directive_string ignore);
  Hashtbl.add
    directive_table
    "use"
    (Toploop.Directive_string
       (fun _ -> failwith "#use is not allowed inside a dune file in OCaml syntax"));
  Hashtbl.add
    directive_table
    "use_mod"
    (Toploop.Directive_string
       (fun _ -> failwith "#use is not allowed inside a dune file in OCaml syntax"))
;;

module V1 = struct
  (*$ begin_vars $*)
  let context = "dummy_context"
  let ocaml_version = "dummy_version"
  let ocamlc_config = []
  let send_target = "dummy_send_target"

  (*$ end_vars $*)

  let send s =
    let oc = open_out_bin send_target in
    output_string oc s;
    close_out oc
  ;;

  let run_and_read_lines cmd =
    let tmp_fname = Filename.temp_file "dune" ".output" in
    at_exit (fun () -> Sys.remove tmp_fname);
    let n = Printf.ksprintf Sys.command "%s > %s" cmd (Filename.quote tmp_fname) in
    let rec loop ic acc =
      match input_line ic with
      | exception End_of_file ->
        close_in ic;
        List.rev acc
      | line -> loop ic (line :: acc)
    in
    let output = loop (open_in tmp_fname) [] in
    if n = 0
    then output
    else
      Printf.ksprintf
        failwith
        "Command failed: %%s\nExit code: %%d\nOutput:\n%%s"
        cmd
        n
        (String.concat "\n" output)
  ;;
end
