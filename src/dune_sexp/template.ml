open Stdune

(* Re-export from Part module for backward compatibility *)
module Pform = Part.Pform
module Part = Part

type t =
  { quoted : bool
  ; parts : Part.t list
  ; loc : Loc.t
  }

let equal { quoted; parts; loc } t =
  Loc.equal loc t.loc && Bool.equal quoted t.quoted && List.equal Part.equal parts t.parts
;;

module Pp : sig
  val to_string : t -> string
end = struct
  let buf = Buffer.create 16

  let add_pform { Pform.loc = _; name; payload } =
    let before, after = "%{", "}" in
    Buffer.add_string buf before;
    Buffer.add_string buf name;
    (match payload with
     | None -> ()
     | Some payload ->
       Buffer.add_char buf ':';
       Buffer.add_string buf (Pform.Payload.to_string payload));
    Buffer.add_string buf after
  ;;

  let check_valid_unquoted s ~loc =
    if not (Atom.is_valid s)
    then Code_error.raise ~loc "Invalid text in unquoted template" [ "s", String s ]
  ;;

  let to_string { parts; quoted; loc } =
    Buffer.clear buf;
    if quoted then Buffer.add_char buf '"';
    let commit_text s =
      if s = ""
      then ()
      else if not quoted
      then (
        check_valid_unquoted ~loc s;
        Buffer.add_string buf s)
      else Buffer.add_string buf (Escape.escaped s)
    in
    let rec add_parts acc_text = function
      | [] -> commit_text acc_text
      | Part.Text s :: rest -> add_parts (if acc_text = "" then s else acc_text ^ s) rest
      | Pform v :: rest ->
        commit_text acc_text;
        add_pform v;
        add_parts "" rest
    in
    add_parts "" parts;
    if quoted then Buffer.add_char buf '"';
    Buffer.contents buf
  ;;
end

let to_string = Pp.to_string
let pp t = Stdune.Pp.verbatim (Pp.to_string t)

let remove_locs t =
  { t with loc = Loc.none; parts = List.map t.parts ~f:Part.remove_locs }
;;

let to_dyn { quoted; parts; loc = _ } =
  Dyn.record [ "quoted", Dyn.bool quoted; "parts", Dyn.list Part.to_dyn parts ]
;;
