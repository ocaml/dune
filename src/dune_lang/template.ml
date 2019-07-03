open! Stdune

include Types.Template

let compare_var_syntax x y =
  match x, y with
  | Percent, Percent
  | Dollar_brace, Dollar_brace
  | Dollar_paren, Dollar_paren -> Ordering.Eq
  | Percent, (Dollar_brace | Dollar_paren) -> Ordering.Lt
  | (Dollar_brace | Dollar_paren), Percent -> Ordering.Gt
  | Dollar_brace, Dollar_paren -> Ordering.Lt
  | Dollar_paren, Dollar_brace -> Ordering.Gt

let compare_var_no_loc v1 v2 =
   match String.compare v1.name v2.name with
   | Ordering.Lt | Gt as a -> a
   | Eq ->
       match Option.compare String.compare v1.payload v2.payload with
       | Ordering.Lt | Gt as a -> a
       | Eq -> compare_var_syntax v1.syntax v2.syntax

let compare_part p1 p2 =
   match p1, p2 with
   | Text s1, Text s2 -> String.compare s1 s2
   | Var v1, Var v2 -> compare_var_no_loc v1 v2
   | Text _, Var _ -> Ordering.Lt
   | Var _, Text _ -> Ordering.Gt

let compare_no_loc t1 t2 =
  match List.compare ~compare:compare_part t1.parts t2.parts with
  | Ordering.Lt | Gt as a -> a
  | Eq -> Bool.compare t1.quoted t2.quoted

let var_enclosers = function
  | Percent      -> "%{", "}"
  | Dollar_brace -> "${", "}"
  | Dollar_paren -> "$(", ")"

module Pp : sig
  val to_string : t -> syntax:File_syntax.t -> string
end = struct
  let buf = Buffer.create 16

  let add_var { loc = _; syntax; name; payload } =
    let before, after = var_enclosers syntax in
    Buffer.add_string buf before;
    Buffer.add_string buf name;
    begin match payload with
    | None -> ()
    | Some payload ->
      Buffer.add_char buf ':';
      Buffer.add_string buf payload
    end;
    Buffer.add_string buf after

  (* TODO use the loc for the error *)
  let check_valid_unquoted s ~syntax ~loc:_ =
    if not (Atom.is_valid (Atom.of_string s) syntax) then
      Code_error.raise "Invalid text in unquoted template"
        ["s", String s]

  let to_string { parts; quoted; loc } ~syntax =
    Buffer.clear buf;
    if quoted then Buffer.add_char buf '"';
    let commit_text s =
      if s = "" then
        ()
      else if not quoted then begin
        check_valid_unquoted ~loc ~syntax s;
        Buffer.add_string buf s
      end else
        Buffer.add_string buf (Escape.escaped ~syntax s)
    in
    let rec add_parts acc_text = function
      | [] ->
        commit_text acc_text
      | Text s :: rest ->
        add_parts (if acc_text = "" then s else acc_text ^ s) rest
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

let string_of_var { loc = _; syntax; name; payload } =
  let before, after = var_enclosers syntax in
  match payload with
  | None -> before ^ name ^ after
  | Some p -> before ^ name ^ ":" ^ p ^ after

let pp syntax t = Stdune.Pp.verbatim (Pp.to_string ~syntax t)

let pp_split_strings ppf (t : t) =
  let syntax = File_syntax.Dune in
  if t.quoted || List.exists t.parts ~f:(function
    | Text s -> String.contains s '\n'
    | Var _ -> false) then begin
    List.iter t.parts ~f:(function
      | Var s ->
        Format.pp_print_string ppf (string_of_var s)
      | Text s ->
        begin match String.split s ~on:'\n' with
        | [] -> assert false
        | [s] -> Format.pp_print_string ppf (Escape.escaped ~syntax s)
        | split ->
          Format.pp_print_list
            ~pp_sep:(fun ppf () -> Format.fprintf ppf "@,\\n")
            Format.pp_print_string ppf
            split
        end
    );
    Format.fprintf ppf "@}\"@]"
  end
  else
    Format.pp_print_string ppf (Pp.to_string ~syntax t)

let remove_locs t =
  { t with
    loc = Loc.none
  ; parts =
      List.map t.parts ~f:(function
        | Var v -> Var { v with loc = Loc.none }
        | Text _ as s -> s)
  }
