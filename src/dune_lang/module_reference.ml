open Import

module T = struct
  type mode =
    | Legacy
    | Path

  type t =
    { loc : Loc.t
    ; path : Module_name.Path.t
    ; mode : mode
    }

  let mode_to_string = function
    | Legacy -> "legacy"
    | Path -> "path"
  ;;

  let compare t1 t2 =
    let open Ordering.O in
    let= () =
      match t1.mode, t2.mode with
      | Legacy, Legacy | Path, Path -> Eq
      | Legacy, Path -> Lt
      | Path, Legacy -> Gt
    in
    Module_name.Path.compare t1.path t2.path
  ;;

  let equal t1 t2 = compare t1 t2 |> Ordering.is_eq

  let to_dyn { loc; path; mode } =
    Dyn.record
      [ "loc", Loc.to_dyn loc
      ; "path", Module_name.Path.to_dyn path
      ; "mode", Dyn.string (mode_to_string mode)
      ]
  ;;
end

include T

let loc t = t.loc
let path t = t.path
let to_string t = Module_name.Path.to_string t.path

let is_qualified t =
  match t.path with
  | _ :: _ :: _ -> true
  | [ _ ] -> false
;;

let is_legacy t =
  match t.mode with
  | Legacy -> true
  | Path -> false
;;

let make ~loc ~mode path = { loc; path; mode }

let parse_component loc component =
  Module_name.of_string_user_error (loc, component) |> User_error.ok_exn
;;

let of_string version (loc, value) =
  let components = String.split value ~on:'.' in
  let mode = if version >= (3, 25) then Path else Legacy in
  if List.length components > 1 && version < (3, 25)
  then
    Syntax.Error.since loc Stanza.syntax (3, 25) ~what:"Using qualified module references";
  let path = List.map components ~f:(parse_component loc) |> Nonempty_list.of_list_exn in
  make ~loc ~mode path
;;

let decode =
  let open Decoder in
  let+ value = located string
  and+ version = Syntax.get_exn Stanza.syntax in
  of_string version value
;;

module Per_item = struct
  module Base = Per_item.Make (T)
  include Base
  open Decoder

  let repr value_repr =
    Repr.view
      Repr.(pair (list (triple String.repr String.repr Int.repr)) (list value_repr))
      ~to_:(fun t ->
        let references, values = enumerate t in
        ( List.map references ~f:(fun (reference, index) ->
            to_string reference, mode_to_string reference.mode, index)
        , values ))
  ;;

  let decode ~default value =
    peek_exn
    >>= function
    | List (loc, Atom (_, A "per_module") :: _) ->
      sum
        [ ( "per_module"
          , let+ mappings =
              repeat
                (let+ value, references = pair value (repeat decode) in
                 references, value)
            in
            of_mapping mappings ~default
            |> function
            | Ok t -> t
            | Error (reference, _, _) ->
              User_error.raise
                ~loc
                [ Pp.textf "module %s present in two different sets" (to_string reference)
                ] )
        ]
    | _ -> value >>| for_all
  ;;

  let mode t =
    match fst (enumerate t) with
    | [] -> Path
    | (reference, _) :: _ -> reference.mode
  ;;

  let find t ~path ~name =
    let mode = mode t in
    let path =
      match mode with
      | Legacy -> Nonempty_list.[ name ]
      | Path -> path
    in
    Base.get t (make ~loc:Loc.none ~mode path)
  ;;

  let explicit_references t = fst (enumerate t) |> List.map ~f:fst
end
