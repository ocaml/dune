open! Stdune
open Import

module Info = struct
  type t =
    | From_dune_file of Loc.t
    | Internal
    | Source_file_copy of Path.Source.t

  let of_loc_opt = function
    | None -> Internal
    | Some loc -> From_dune_file loc

  let to_dyn : t -> Dyn.t = function
    | From_dune_file loc -> Dyn.Variant ("From_dune_file", [ Loc.to_dyn loc ])
    | Internal -> Dyn.Variant ("Internal", [])
    | Source_file_copy p ->
      Dyn.Variant ("Source_file_copy", [ Path.Source.to_dyn p ])
end

module Promote = struct
  module Lifetime = struct
    type t =
      | Unlimited
      | Until_clean
  end

  module Into = struct
    type t =
      { loc : Loc.t
      ; dir : string
      }
  end

  type t =
    { lifetime : Lifetime.t
    ; into : Into.t option
    ; only : Predicate_lang.Glob.t option
    }
end

module Mode = struct
  type t =
    | Standard
    | Fallback
    | Promote of Promote.t
    | Ignore_source_files
end

module Id = Id.Make ()

type facts_or_deps =
  | Facts of Dep.Facts.t
  | Deps of Dep.Set.t

module T = struct
  type t =
    { id : Id.t
    ; context : Build_context.t option
    ; targets : Path.Build.Set.t
    ; action : (Action.Full.t * facts_or_deps) Memo.Lazy.t
    ; mode : Mode.t
    ; info : Info.t
    ; loc : Loc.t
    ; dir : Path.Build.t
    }

  let compare a b = Id.compare a.id b.id

  let equal a b = Id.equal a.id b.id

  let hash t = Id.hash t.id

  let loc t = t.loc

  let to_dyn t : Dyn.t =
    Record [ ("id", Id.to_dyn t.id); ("info", Info.to_dyn t.info) ]
end

include T
module O = Comparable.Make (T)
module Set = O.Set

let make ?(sandbox = Sandbox_config.default) ?(mode = Mode.Standard) ~context
    ?(info = Info.Internal) ~targets action =
  let open Memo.Build.O in
  let action =
    Memo.lazy_ ~name:"Rule.make" (fun () ->
        let+ action, facts_or_deps = action in
        let dep = Dep.sandbox_config sandbox in
        let facts_or_deps =
          match facts_or_deps with
          | Facts facts -> Facts (Dep.Map.add_exn facts dep Dep.Fact.nothing)
          | Deps deps -> Deps (Dep.Set.add deps dep)
        in
        (action, facts_or_deps))
  in
  let dir =
    match Path.Build.Set.choose targets with
    | None -> (
      match info with
      | From_dune_file loc ->
        User_error.raise ~loc [ Pp.text "Rule has no targets specified" ]
      | _ -> Code_error.raise "Build_interpret.Rule.make: no targets" [])
    | Some x ->
      let dir = Path.Build.parent_exn x in
      (if
       Path.Build.Set.exists targets ~f:(fun path ->
           Path.Build.( <> ) (Path.Build.parent_exn path) dir)
      then
        match info with
        | Internal
        | Source_file_copy _ ->
          Code_error.raise "rule has targets in different directories"
            [ ("targets", Path.Build.Set.to_dyn targets) ]
        | From_dune_file loc ->
          User_error.raise ~loc
            [ Pp.text "Rule has targets in different directories.\nTargets:"
            ; Pp.enumerate (Path.Build.Set.to_list targets) ~f:(fun p ->
                  Pp.verbatim (Path.to_string_maybe_quoted (Path.build p)))
            ]);
      dir
  in
  let loc =
    match info with
    | From_dune_file loc -> loc
    | Internal ->
      Loc.in_file
        (Path.drop_optional_build_context
           (Path.build (Path.Build.relative dir "_unknown_")))
    | Source_file_copy p -> Loc.in_file (Path.source p)
  in
  { id = Id.gen (); targets; context; action; mode; info; loc; dir }

let set_action t action = { t with action }

let find_source_dir rule =
  let _, src_dir = Path.Build.extract_build_context_dir_exn rule.dir in
  Source_tree.nearest_dir src_dir

module Anonymous_action = struct
  type t =
    { context : Build_context.t option
    ; action : Action.Full.t
    ; loc : Loc.t option
    ; dir : Path.Build.t
    ; alias : Alias.Name.t option
    }
end
