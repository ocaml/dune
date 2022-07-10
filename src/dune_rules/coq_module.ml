(* (c) MINES ParisTech 2018-2019               *)
(* (c) INRIA 2020                              *)
(* Written by: Emilio Jesús Gallego Arias *)

open Import

module Name = struct
  type t = string

  let make x = x

  let compare = String.compare

  let equal = String.equal

  let to_dyn s = Dyn.String s

  let to_string s = s

  module Map = String.Map

  module Path = struct
    module T = struct
      type nonrec t = t list

      let compare = List.compare ~compare

      let to_dyn = Dyn.list to_dyn
    end

    include T

    let rec is_prefix t ~prefix =
      match (t, prefix) with
      | [], _ | _, [] -> true
      | x :: xs, p :: prefix -> equal x p && is_prefix xs ~prefix

    let to_list x = x

    let rev = List.rev

    let empty = []

    let append_name t name = t @ [ name ]

    let to_string_list t = List.map ~f:to_string t

    let of_string_list = List.map ~f:make

    let of_string s = String.split_on_char ~sep:'.' s |> of_string_list

    let of_lib_name lib_name = of_string (Coq_lib_name.to_string lib_name)

    module C = Comparable.Make (T)
    module Map = C.Map
  end
end

module Source = struct
  type t =
    { source : Path.Build.t
    ; prefix : Name.Path.t
    ; name : Name.t
    }

  let compare { source; prefix; name } t =
    let open Ordering.O in
    let= () = Path.Build.compare source t.source in
    let= () = Name.Path.compare prefix t.prefix in
    Name.compare name t.name

  let to_dyn { source; prefix; name } =
    Dyn.record
      [ ("source", Path.Build.to_dyn source)
      ; ("prefix", Name.Path.to_dyn prefix)
      ; ("name", Name.to_dyn name)
      ]

  let make ~source ~prefix ~name = { source; prefix; name }
end

module Module = struct
  type t =
    { source : Source.t
    ; theory_prefix : Name.Path.t
    ; obj_dir : Path.Build.t
    }

  let compare { source; theory_prefix; obj_dir } t =
    let open Ordering.O in
    let= () = Source.compare source t.source in
    let= () = Name.Path.compare theory_prefix t.theory_prefix in
    Path.Build.compare obj_dir t.obj_dir

  let to_dyn { source; theory_prefix; obj_dir } =
    Dyn.record
      [ ("source", Source.to_dyn source)
      ; ("theory_prefix", Name.Path.to_dyn theory_prefix)
      ; ("obj_dir", Path.Build.to_dyn obj_dir)
      ]
end

include Module
module Map = Map.Make (Module)

let make ~source ~prefix ~name ~theory_prefix ~obj_dir =
  { theory_prefix; source = Source.make ~source ~prefix ~name; obj_dir }

let prefix x = x.source.prefix

let name x = x.source.name

let theory_prefix x = x.theory_prefix

let obj_dir x = x.obj_dir

let source x = x.source.source

let path x = x.theory_prefix @ x.source.prefix @ [ x.source.name ]

let of_source source ~obj_dir ~theory =
  { source; theory_prefix = Name.Path.of_lib_name theory; obj_dir }

let build_vo_dir x =
  List.fold_left (prefix x) ~init:x.obj_dir ~f:Path.Build.relative

let cmxs_of_mod ~wrapper_name x =
  let wrapper_split = String.split wrapper_name ~on:'.' in
  let native_base =
    "N"
    ^ String.concat ~sep:"_"
        (wrapper_split @ x.source.prefix @ [ x.source.name ])
  in
  [ native_base ^ Cm_kind.ext Cmi; native_base ^ Mode.plugin_ext Native ]

let dep_file x =
  let vo_dir = build_vo_dir x in
  Path.Build.relative vo_dir (name x ^ ".v.d")

let glob_file x =
  let vo_dir = build_vo_dir x in
  Path.Build.relative vo_dir (name x ^ ".glob")

let vo_file x =
  let vo_dir = build_vo_dir x in
  Path.Build.relative vo_dir (name x ^ ".vo")

type target =
  | Vo
  | Vos

type obj_files_mode =
  | Build of target
  | Install

(* XXX: Remove the install .coq-native hack once rules can output targets in
   multiple subdirs *)
let obj_files x ~wrapper_name ~mode ~obj_files_mode =
  let vo_dir = build_vo_dir x in
  let install_vo_dir = String.concat ~sep:"/" (prefix x) in
  let native_objs =
    match mode with
    | Coq_mode.Native ->
      let cmxs_obj = cmxs_of_mod ~wrapper_name x in
      List.map
        ~f:(fun x ->
          ( Path.Build.relative vo_dir x
          , Filename.(concat (concat install_vo_dir ".coq-native") x) ))
        cmxs_obj
    | VoOnly | Legacy -> []
  in
  let obj_files =
    match obj_files_mode with
    | Build Vo -> [ name x ^ ".vo"; name x ^ ".glob" ]
    | Build Vos -> [ name x ^ ".vos" ]
    | Install -> [ name x ^ ".vo" ]
  in
  List.map obj_files ~f:(fun fname ->
      (Path.Build.relative vo_dir fname, Filename.concat install_vo_dir fname))
  @ native_objs

let equal x y = Ordering.is_eq (compare x y)

let parse ~dir ~loc ~theory_prefix ~obj_dir s =
  (* TODO parsing incorrect, need to find out theory name *)
  let clist = List.rev @@ String.split s ~on:'.' in
  match clist with
  | [] -> User_error.raise ~loc [ Pp.text "Coq module name cannot be empty." ]
  | name :: prefix ->
    let prefix = List.rev prefix in
    let source = List.fold_left prefix ~init:dir ~f:Path.Build.relative in
    let source = Path.Build.relative source (name ^ ".v") in
    make ~name ~source ~prefix ~theory_prefix ~obj_dir

let eval =
  let key x = String.concat ~sep:"." (prefix x @ [ name x ]) in
  let eq_key x y = String.equal (key x) (key y) in
  fun ~dir ~standard ~theory_prefix ~obj_dir osl ->
    Ordered_set_lang.eval
      ~parse:(parse ~dir ~theory_prefix ~obj_dir)
      ~standard ~eq:eq_key osl

module Path = Name.Path
