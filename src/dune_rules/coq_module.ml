open! Dune_engine

(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* Written by: Emilio Jesús Gallego Arias *)

open! Stdune

module Name = struct
  type t = string

  let make x = x

  let compare = String.compare

  let equal = String.equal

  let to_dyn s = Dyn.String s

  let to_string s = s
end

(* We keep prefix and name separated as the handling of `From Foo Require Bar.`
   may benefit from it. *)
type t =
  { source : Path.Build.t
  ; prefix : string list
  ; name : Name.t
  }

let make ~source ~prefix ~name = { source; prefix; name }

let source x = x.source

let prefix x = x.prefix

let name x = x.name

let build_vo_dir ~obj_dir x =
  List.fold_left x.prefix ~init:obj_dir ~f:Path.Build.relative

type obj =
  | Dep
  | Aux
  | Glob
  | Obj

let fname_of_obj t obj =
  match obj with
  | Dep -> t.name ^ ".v.d"
  | Aux -> "." ^ t.name ^ ".aux"
  | Glob -> t.name ^ ".glob"
  | Obj -> t.name ^ ".vo"

let obj_file t obj ~obj_dir =
  let vo_dir = build_vo_dir ~obj_dir t in
  let fname = fname_of_obj t obj in
  Path.Build.relative vo_dir fname

let to_dyn { source; prefix; name } =
  let open Dyn.Encoder in
  record
    [ ("source", Path.Build.to_dyn source)
    ; ("prefix", list string prefix)
    ; ("name", Name.to_dyn name)
    ]

let parse ~dir ~loc s =
  let clist = List.rev @@ String.split s ~on:'.' in
  match clist with
  | [] -> User_error.raise ~loc [ Pp.text "Invalid coq module" ]
  | name :: prefix ->
    let prefix = List.rev prefix in
    let source = List.fold_left prefix ~init:dir ~f:Path.Build.relative in
    let source = Path.Build.relative source (name ^ ".v") in
    make ~name ~source ~prefix

let eval =
  let key x = String.concat ~sep:"." (x.prefix @ [ x.name ]) in
  let eq_key x y = String.equal (key x) (key y) in
  fun ~dir ~standard osl ->
    Ordered_set_lang.eval ~parse:(parse ~dir) ~standard ~eq:eq_key osl
