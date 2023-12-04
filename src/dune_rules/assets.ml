let jbuild_plugin_ml = {jbp|
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
|jbp}

let jbuild_plugin_mli = {jbp|(** API for jbuild plugins *)

(* CR-someday amokhov: rename to [dune_plugin]. *)

module V1 : sig
  (** Current build context *)
  val context : string

  (** OCaml version for the current build context. It might not be the same as
      [Sys.ocaml_version] *)
  val ocaml_version : string

  (** Output of [ocamlc -config] for this context *)
  val ocamlc_config : (string * string) list

  (** [send s] send [s] to Dune. [s] should be the contents of a [dune] file
      following the specification described in the manual. *)
  val send : string -> unit

  (** Execute a command and read its output *)
  val run_and_read_lines : string -> string list
end
|jbp}