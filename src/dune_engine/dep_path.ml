open! Stdune

module Entry = struct
  module Lib = struct
    type t =
      { path : Path.t
      ; name : Lib_name.t
      }

    let pp { path; name } =
      Pp.textf "library %S in %s" (Lib_name.to_string name)
        (Path.to_string_maybe_quoted path)
  end

  module Implements_via = struct
    type t =
      | Variant of Variant.t
      | Default_for of Lib.t

    let pp = function
      | Variant v -> Pp.textf "via variant %S" (Variant.to_string v)
      | Default_for l ->
        Pp.seq (Pp.text "via default implementation for ") (Lib.pp l)
  end

  type t =
    | Path of Path.t
    | Alias of (Loc.t * Alias.t)
    | Library of Lib.t * Implements_via.t option
    | Executables of (Loc.t * string) list
    | Preprocess of Lib_name.t list
    | Loc of Loc.t

  let pp = function
    | Path p -> Pp.text (Dpath.describe_path p)
    | Alias (loc, a) ->
      let loc_suffix =
        if Loc.is_none loc then
          ""
        else
          " in " ^ Loc.to_file_colon_line loc
      in
      Pp.textf "alias %s%s" (Alias.describe a) loc_suffix
    | Library (lib, via) -> (
      match via with
      | None -> Lib.pp lib
      | Some via ->
        Pp.concat ~sep:Pp.space [ Lib.pp lib; Implements_via.pp via ] )
    | Executables [ (loc, name) ] ->
      Pp.textf "executable %s in %s" name (Loc.to_file_colon_line loc)
    | Executables names ->
      let loc, _ = List.hd names in
      Pp.textf "executables %s in %s"
        (String.enumerate_and (List.map ~f:snd names))
        (Loc.to_file_colon_line loc)
    | Preprocess l ->
      Pp.textf "%s"
        (Dyn.to_string
           (List [ String "pps"; Dyn.Encoder.(list Lib_name.to_dyn) l ]))
    | Loc loc -> Pp.text (Loc.to_file_colon_line loc)
end

module Entries = struct
  type t = Entry.t list

  let pp t =
    Pp.vbox
      (Pp.concat ~sep:Pp.cut
         (List.map t ~f:(fun x ->
              Pp.box ~indent:3
                (Pp.seq (Pp.verbatim "-> ")
                   (Pp.seq (Pp.text "required by ") (Entry.pp x))))))
end

exception E of exn * Entry.t list

let () =
  Memo.unwrap_exn :=
    function
    | E (exn, _) -> exn
    | exn -> exn

let prepend_exn exn entry =
  match exn with
  | E (exn, entries) -> E (exn, entry :: entries)
  | exn -> E (exn, [ entry ])

let reraise exn entry =
  Exn_with_backtrace.map_and_reraise exn ~f:(fun exn -> prepend_exn exn entry)

let is_loc_none loc =
  match loc with
  | None -> true
  | Some loc -> Loc.is_none loc

let recover_loc (entries : Entry.t list) =
  match entries with
  (* In principle it makes sense to recover loc for more than just aliases, but
     for the sake of preserving behavior we're minimizing the effect of this
     feature, in particular to avoid overlap with [Rule_fn.loc ()] in
     build_system.ml, which serves a similar purpose. *)
  | Alias (loc, _) :: _ -> Some loc
  | _ -> None

let augment_user_error_loc entries exn =
  match exn with
  | User_error.E msg ->
    if is_loc_none msg.loc then
      match recover_loc entries with
      | None -> exn
      | Some loc -> User_error.E { msg with loc = Some loc }
    else
      exn
  | _ -> exn

let unwrap_exn = function
  | E (exn, entries) ->
    let exn = augment_user_error_loc entries exn in
    (exn, Some entries)
  | exn -> (exn, None)

let map ~f = function
  | E (exn, entries) -> (
    match f exn with
    | E (exn, entries') -> E (exn, entries' @ entries)
    | exn -> E (exn, entries) )
  | exn -> f exn

let () =
  Printexc.register_printer (function
    | E (exn, _) -> Some (Printexc.to_string exn)
    | _ -> None)
