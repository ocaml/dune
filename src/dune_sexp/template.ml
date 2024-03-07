open! Stdune
module Format = Stdlib.Format

module Pform = struct
  module Payload = struct
    type t = string

    let of_string t = t
    let to_string t = t
    let to_dyn = Dyn.string
    let compare = String.compare

    module Args = struct
      let sep = ':'
      let whole t = t

      let lsplit2 t loc =
        match String.lsplit2 t ~on:sep with
        | Some args -> Ok args
        | None ->
          Error
            (User_error.make
               ~loc
               [ Pp.textf
                   "Expected two arguments separated by '%c' but no '%c' found."
                   sep
                   sep
               ])
      ;;

      let split t = String.split t ~on:sep
    end

    let of_args = String.concat ~sep:(String.make 1 Args.sep)
  end

  type t =
    { loc : Loc.t
    ; name : string
    ; payload : Payload.t option
    }

  let loc (t : t) = t.loc

  let payload_loc t =
    if Option.is_some t.payload
    then (
      let loc_with_start =
        Loc.set_start
          t.loc
          { (Loc.start t.loc) with
            (* Add 3 for the "%{" at the start of the pform and then ":"
               separating the name from the payload *)
            pos_cnum = Loc.start_pos_cnum t.loc + String.length t.name + 3
          }
      in
      Loc.set_stop
        loc_with_start
        { (Loc.stop loc_with_start) with
          (* Subtract 1 for the "}" at the end of the pform *)
          pos_cnum = Loc.stop_pos_cnum loc_with_start - 1
        })
    else Loc.none
  ;;

  let name { name; _ } = name

  let compare_no_loc { name; payload; loc = _ } t =
    let open Ordering.O in
    let= () = String.compare name t.name in
    Option.compare Payload.compare payload t.payload
  ;;

  let compare { name; payload; loc } t =
    match String.compare name t.name with
    | (Lt | Gt) as x -> x
    | Eq ->
      (match Option.compare Payload.compare payload t.payload with
       | (Lt | Gt) as x -> x
       | Eq -> Loc.compare loc t.loc)
  ;;

  let full_name t =
    match t.payload with
    | None -> t.name
    | Some v -> t.name ^ ":" ^ Payload.to_string v
  ;;

  let payload t = t.payload

  let to_string { loc = _; name; payload } =
    let before, after = "%{", "}" in
    match payload with
    | None -> before ^ name ^ after
    | Some p -> before ^ name ^ ":" ^ Payload.to_string p ^ after
  ;;

  let to_dyn { loc = _; name; payload } =
    let open Dyn in
    record [ "name", string name; "payload", option Payload.to_dyn payload ]
  ;;

  let with_name t ~name = { t with name }

  let describe t =
    to_string
      (match t.payload with
       | None -> t
       | Some _ -> { t with payload = Some (Payload.of_string "..") })
  ;;

  let describe_kind t =
    match t.payload with
    | None -> "variable"
    | Some _ -> "macro"
  ;;
end

type part =
  | Text of string
  | Pform of Pform.t

type t =
  { quoted : bool
  ; parts : part list
  ; loc : Loc.t
  }

let compare_part_no_loc p1 p2 =
  match p1, p2 with
  | Text s1, Text s2 -> String.compare s1 s2
  | Pform v1, Pform v2 -> Pform.compare_no_loc v1 v2
  | Text _, Pform _ -> Ordering.Lt
  | Pform _, Text _ -> Ordering.Gt
;;

let compare_part p1 p2 =
  match p1, p2 with
  | Text s1, Text s2 -> String.compare s1 s2
  | Pform v1, Pform v2 -> Pform.compare v1 v2
  | Text _, Pform _ -> Ordering.Lt
  | Pform _, Text _ -> Ordering.Gt
;;

let compare_no_loc { quoted; parts; loc = _ } t =
  let open Ordering.O in
  let= () = List.compare ~compare:compare_part_no_loc parts t.parts in
  Bool.compare quoted t.quoted
;;

let compare { quoted; parts; loc } t =
  match Bool.compare t.quoted quoted with
  | (Lt | Gt) as x -> x
  | Eq ->
    (match List.compare ~compare:compare_part parts t.parts with
     | (Lt | Gt) as x -> x
     | Eq -> Loc.compare loc t.loc)
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
      | Text s :: rest -> add_parts (if acc_text = "" then s else acc_text ^ s) rest
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

let pp_split_strings ppf (t : t) =
  if t.quoted
     || List.exists t.parts ~f:(function
       | Text s -> String.contains s '\n'
       | Pform _ -> false)
  then (
    List.iter t.parts ~f:(function
      | Pform s -> Format.pp_print_string ppf (Pform.to_string s)
      | Text s ->
        (match String.split s ~on:'\n' with
         | [] -> assert false
         | [ s ] -> Format.pp_print_string ppf (Escape.escaped s)
         | split ->
           Format.pp_print_list
             ~pp_sep:(fun ppf () -> Format.fprintf ppf "@,\\n")
             Format.pp_print_string
             ppf
             split));
    Format.fprintf ppf "@}\"@]")
  else Format.pp_print_string ppf (Pp.to_string t)
;;

let remove_locs t =
  { t with
    loc = Loc.none
  ; parts =
      List.map t.parts ~f:(function
        | Pform v -> Pform { v with loc = Loc.none }
        | Text _ as s -> s)
  }
;;

let dyn_of_part =
  let open Dyn in
  function
  | Text s -> variant "Text" [ string s ]
  | Pform v -> variant "Pform" [ Pform.to_dyn v ]
;;

let to_dyn { quoted; parts; loc = _ } =
  let open Dyn in
  record [ "quoted", bool quoted; "parts", list dyn_of_part parts ]
;;
