(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(*     Written by: Emilio Jesús Gallego Arias  *)

open! Stdune

module Name = struct

  type t = string

  let make x = x
  let compare = String.compare
  let pp = Fmt.text

end

(* We keep prefix and name separated as the handling of
  `From Foo Require Bar.` may benefit from it. *)
type t =
  { source: Path.Build.t
  ; prefix : string list
  ; name : string
  }

let make ~source ~prefix ~name =
  { source
  ; prefix
  ; name
  }

let source x = x.source
let prefix x = x.prefix
let name x = x.name
let obj_file ~obj_dir ~ext x =
  let vo_dir = List.fold_left x.prefix ~init:obj_dir ~f:Path.Build.relative in
  Path.Build.relative vo_dir (x.name ^ ext)
let pp fmt x =
  let open Format in
  let pp_sep fmt () = pp_print_string fmt "." in
  fprintf fmt "{ prefix = %a; name = %s; source = %a }"
    (pp_print_list ~pp_sep pp_print_string) x.prefix x.name
    Path.Build.pp x.source

let parse ~dir ~loc s =
  let clist = List.rev @@ String.split s ~on:'.' in
  match clist with
  | [] ->
    Errors.fail loc "invalid coq module"
  | name :: prefix ->
    let prefix = List.rev prefix in
    let source = List.fold_left prefix ~init:dir ~f:Path.Build.relative in
    let source = Path.Build.relative source (name ^ ".v") in
    make ~name ~source ~prefix

module Value = struct
  type nonrec t = t
  type key = string
  let key x = String.concat ~sep:"." (x.prefix @ [x.name])
end

module Eval = Ordered_set_lang.Make(String)(Value)
