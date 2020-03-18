open! Stdune

type var =
  { loc : Loc.t
  ; name : string
  ; payload : string option
  }

type part =
  | Text of string
  | Var of var

type t =
  { quoted : bool
  ; parts : part list
  ; loc : Loc.t
  }

let compare_var_no_loc v1 v2 =
  match String.compare v1.name v2.name with
  | (Ordering.Lt | Gt) as a -> a
  | Eq -> Option.compare String.compare v1.payload v2.payload

let compare_part p1 p2 =
  match (p1, p2) with
  | Text s1, Text s2 -> String.compare s1 s2
  | Var v1, Var v2 -> compare_var_no_loc v1 v2
  | Text _, Var _ -> Ordering.Lt
  | Var _, Text _ -> Ordering.Gt

let compare_no_loc t1 t2 =
  match List.compare ~compare:compare_part t1.parts t2.parts with
  | (Ordering.Lt | Gt) as a -> a
  | Eq -> Bool.compare t1.quoted t2.quoted

module Pp : sig
  val to_string : t -> string
end = struct
  let buf = Buffer.create 16

  let add_var { loc = _; name; payload } =
    let before, after = ("%{", "}") in
    Buffer.add_string buf before;
    Buffer.add_string buf name;
    ( match payload with
    | None -> ()
    | Some payload ->
      Buffer.add_char buf ':';
      Buffer.add_string buf payload );
    Buffer.add_string buf after

  let check_valid_unquoted s ~loc =
    if not (Atom.is_valid s) then
      Code_error.raise ~loc "Invalid text in unquoted template"
        [ ("s", String s) ]

  let to_string { parts; quoted; loc } =
    Buffer.clear buf;
    if quoted then Buffer.add_char buf '"';
    let commit_text s =
      if s = "" then
        ()
      else if not quoted then (
        check_valid_unquoted ~loc s;
        Buffer.add_string buf s
      ) else
        Buffer.add_string buf (Escape.escaped s)
    in
    let rec add_parts acc_text = function
      | [] -> commit_text acc_text
      | Text s :: rest ->
        add_parts
          ( if acc_text = "" then
            s
          else
            acc_text ^ s )
          rest
      | Var v :: rest ->
        commit_text acc_text;
        add_var v;
        add_parts "" rest
    in
    add_parts "" parts;
    if quoted then Buffer.add_char buf '"';
    Buffer.contents buf
end

let to_string = Pp.to_string

let string_of_var { loc = _; name; payload } =
  let before, after = ("%{", "}") in
  match payload with
  | None -> before ^ name ^ after
  | Some p -> before ^ name ^ ":" ^ p ^ after

let pp t = Stdune.Pp.verbatim (Pp.to_string t)

let pp_split_strings ppf (t : t) =
  if
    t.quoted
    || List.exists t.parts ~f:(function
         | Text s -> String.contains s '\n'
         | Var _ -> false)
  then (
    List.iter t.parts ~f:(function
      | Var s -> Format.pp_print_string ppf (string_of_var s)
      | Text s -> (
        match String.split s ~on:'\n' with
        | [] -> assert false
        | [ s ] -> Format.pp_print_string ppf (Escape.escaped s)
        | split ->
          Format.pp_print_list
            ~pp_sep:(fun ppf () -> Format.fprintf ppf "@,\\n")
            Format.pp_print_string ppf split ));
    Format.fprintf ppf "@}\"@]"
  ) else
    Format.pp_print_string ppf (Pp.to_string t)

let remove_locs t =
  { t with
    loc = Loc.none
  ; parts =
      List.map t.parts ~f:(function
        | Var v -> Var { v with loc = Loc.none }
        | Text _ as s -> s)
  }

let dyn_of_var { loc = _; name; payload } =
  let open Dyn.Encoder in
  record [ ("name", string name); ("payload", option string payload) ]

let dyn_of_part =
  let open Dyn.Encoder in
  function
  | Text s -> constr "Text" [ string s ]
  | Var v -> constr "Var" [ dyn_of_var v ]

let to_dyn { quoted; parts; loc = _ } =
  let open Dyn.Encoder in
  record [ ("quoted", bool quoted); ("parts", list dyn_of_part parts) ]
