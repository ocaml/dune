open Import

module T : sig
  type t = private
    { main : User_message.t
    ; related : User_message.t list
    }

  val create : main:User_message.t -> related:User_message.t list -> t
end = struct
  type t =
    { main : User_message.t
    ; related : User_message.t list
    }

  let create ~main ~related =
    let () =
      List.iter related ~f:(fun (related : User_message.t) ->
        match related.loc with
        | Some _ -> ()
        | None ->
          Code_error.raise
            "related messages must have locations"
            [ "related", String (User_message.to_string related) ])
    in
    { main; related }
  ;;
end

include T

let to_dyn { main; related } =
  let open Dyn in
  record
    [ "main", string (User_message.to_string main)
    ; "related", (list string) (List.map related ~f:User_message.to_string)
    ]
;;

let annot = User_message.Annots.Key.create ~name:"compound-user-error" (Dyn.list to_dyn)
let make ~main ~related = create ~main ~related

let make_loc ~dir { Ocamlc_loc.path; chars; lines } : Loc.t =
  let pos_fname =
    let dir = Path.drop_optional_build_context_maybe_sandboxed dir in
    Path.to_absolute_filename (Path.relative dir path)
  in
  let pos_lnum_start, pos_lnum_stop =
    match lines with
    | Single i -> i, i
    | Range (i, j) -> i, j
  in
  let pos_cnum_start, pos_cnum_stop =
    match chars with
    | None -> 0, 0
    | Some (x, y) -> x, y
  in
  let pos = { Lexing.pos_fname; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 } in
  let start = { pos with pos_lnum = pos_lnum_start; pos_cnum = pos_cnum_start } in
  let stop = { pos with pos_lnum = pos_lnum_stop; pos_cnum = pos_cnum_stop } in
  Loc.create ~start ~stop
;;

(* There are various situations in which we can do something more helpful that
   the raw output the compiler is able to provide. *)
let override_report rule_loc report =
  match (report : Ocamlc_loc.report), (rule_loc : Loc.t option) with
  | ( { severity = Alert { name = "ocaml_deprecated_auto_include"; _ }
      ; loc = { path = "_none_"; _ }
      ; _
      }
    , Some rule_loc ) ->
    { report with
      loc = { report.loc with path = (Loc.to_lexbuf_loc rule_loc).start.pos_fname }
    }
  | _ -> report
;;

let parse_output ~rule_loc ~dir s =
  Ocamlc_loc.parse s
  |> List.map ~f:(fun (report : Ocamlc_loc.report) ->
    let report = override_report rule_loc report in
    let make_message (loc, message) =
      let loc = make_loc ~dir loc in
      let message = Pp.verbatim message in
      User_message.make ~loc [ message ]
    in
    let main = make_message (report.loc, report.message) in
    let related = List.map report.related ~f:make_message in
    make ~main ~related)
;;
