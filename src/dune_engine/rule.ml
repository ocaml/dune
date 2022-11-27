open Import
module Action_builder = Action_builder0

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
    ; only : Filename.t Predicate.t option
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

module T = struct
  type t =
    { id : Id.t
    ; context : Build_context.t option
    ; targets : Targets.Validated.t
    ; action : Action.Full.t Action_builder.t
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
include Comparable.Make (T)

let make ?(mode = Mode.Standard) ~context ?(info = Info.Internal) ~targets
    action =
  let action = Action_builder.memoize "Rule.make" action in
  let report_error ?(extra_pp = []) message =
    match info with
    | From_dune_file loc ->
      let pp = [ Pp.text message ] @ extra_pp in
      User_error.raise ~loc pp
    | Internal | Source_file_copy _ ->
      Code_error.raise message
        [ ("info", Info.to_dyn info); ("targets", Targets.to_dyn targets) ]
  in
  (* CR-someday amokhov: Since [dir] and [targets] are produced together and are
     logically related, we could make [dir] a field of [Targets.Validated.t]. *)
  let dir, targets =
    match Targets.validate targets with
    | Valid { parent_dir; targets } -> (parent_dir, targets)
    | No_targets -> report_error "Rule has no targets specified"
    | Inconsistent_parent_dir ->
      (* user written actions have their own validation step that also works
         with the target inference mechanism *)
      Code_error.raise "Rule has targets in different directories."
        [ ("targets", Targets.to_dyn targets) ]
    | File_and_directory_target_with_the_same_name path ->
      report_error
        (sprintf "%S is declared as both a file and a directory target."
           (Dpath.describe_target path))
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

let set_action t action =
  let action = Action_builder.memoize "Rule.set_action" action in
  { t with action }

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
