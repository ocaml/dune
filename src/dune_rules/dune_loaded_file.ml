open! Dune_engine
open! Stdune
open Import

type t =
  { dir : Path.Source.t
  ; project : Dune_project.t
  ; stanzas : Dune_file.Stanzas.t
  }

let parse sexps ~dir ~file ~project =
  let stanzas = Dune_file.Stanzas.parse ~file project sexps in
  let stanzas =
    if !Clflags.ignore_promoted_rules then
      List.filter stanzas ~f:(function
        | Dune_file.Rule { mode = Rule.Mode.Promote { only = None; _ }; _ }
        | Dune_file.Menhir.T { mode = Rule.Mode.Promote { only = None; _ }; _ }
          ->
          false
        | _ -> true)
    else
      stanzas
  in
  { dir; project; stanzas }

let rec fold_stanzas l ~init ~f =
  match l with
  | [] -> init
  | t :: l -> inner_fold t t.stanzas l ~init ~f

and inner_fold t inner_list l ~init ~f =
  match inner_list with
  | [] -> fold_stanzas l ~init ~f
  | x :: inner_list -> inner_fold t inner_list l ~init:(f t x init) ~f
