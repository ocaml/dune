open Import
module Action_builder = Dune_engine.Action_builder
include Dune_engine.Action_builder
open O

(* CR-someday amokhov: The set of targets is accumulated using information from
   multiple sources by calling [Targets.combine], which performs set union and
   hence duplicate declarations of the very same target can go unnoticed. I
   think such redeclarations are not erroneous but are merely redundant; perhaps
   we should detect and disallow them. *)
type nonrec 'a t =
  { build : 'a Action_builder.t
  ; targets : Targets.t
  }

let map_build t ~f = { t with build = f t.build }
let return x = { build = return x; targets = Targets.empty }

let add t ~file_targets =
  { build = t.build
  ; targets =
      Targets.combine
        t.targets
        (Targets.Files.create (Path.Build.Set.of_list file_targets))
  }
;;

let add_directories t ~directory_targets =
  { build = t.build
  ; targets =
      Targets.combine
        t.targets
        (Targets.create
           ~dirs:(Path.Build.Set.of_list directory_targets)
           ~files:Path.Build.Set.empty)
  }
;;

let map { build; targets } ~f = { build = map build ~f; targets }

let map2 x y ~f =
  { build = map2 x.build y.build ~f; targets = Targets.combine x.targets y.targets }
;;

let both x y =
  { build = both x.build y.build; targets = Targets.combine x.targets y.targets }
;;

let seq x y =
  { build = x.build >>> y.build; targets = Targets.combine x.targets y.targets }
;;

module O = struct
  let ( >>> ) = seq
  let ( >>| ) t f = map t ~f
  let ( and+ ) = both
  let ( let+ ) a f = map ~f a
end

open O

let all xs =
  match xs with
  | [] -> return []
  | xs ->
    let build, targets =
      Stdune.List.fold_left xs ~init:([], Targets.empty) ~f:(fun (builds, targets) x ->
        x.build :: builds, Targets.combine x.targets targets)
    in
    { build = all (Stdune.List.rev build); targets }
;;

let write_file_dyn ?(perm = Action.File_perm.Normal) fn s =
  add
    ~file_targets:[ fn ]
    (let+ s = s in
     Action.Full.make (Action.Write_file (fn, perm, s)))
;;

let memoize name t = { build = memoize name t.build; targets = t.targets }
