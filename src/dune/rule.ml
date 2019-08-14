open! Stdune
open Import

module Info = struct
  type t =
    | From_dune_file of Loc.t
    | Internal
    | Source_file_copy

  let of_loc_opt = function None -> Internal | Some loc -> From_dune_file loc

  let loc = function
    | From_dune_file loc ->
      Some loc
    | Internal | Source_file_copy ->
      None
end

type t =
  { context : Context.t option
  ; env : Env.t option
  ; build : (unit, Action.t) Build.t
  ; targets : Path.Build.Set.t
  ; mode : Dune_file.Rule.Mode.t
  ; locks : Path.t list
  ; info : Info.t
  ; dir : Path.Build.t
  }

let make ?(sandbox = Sandbox_config.default)
  ?(mode = Dune_file.Rule.Mode.Standard) ~context ~env ?(locks = [])
    ?(info = Info.Internal) build =
  let build = Build.S.seq (Build.dep (Dep.sandbox_config sandbox)) build in
  let targets = Build.targets build in
  let dir =
    match Path.Build.Set.choose targets with
    | None -> (
      match info with
      | From_dune_file loc ->
        User_error.raise ~loc [ Pp.text "Rule has no targets specified" ]
      | _ ->
        Code_error.raise "Build_interpret.Rule.make: no targets" [] )
    | Some x ->
      let dir = Path.Build.parent_exn x in
      ( if
        Path.Build.Set.exists targets ~f:(fun path ->
          Path.Build.( <> ) (Path.Build.parent_exn path) dir)
      then
        match info with
        | Internal | Source_file_copy ->
          Code_error.raise "rule has targets in different directories"
            [ ("targets", Path.Build.Set.to_dyn targets) ]
        | From_dune_file loc ->
          User_error.raise ~loc
            [ Pp.text "Rule has targets in different directories.\nTargets:"
            ; Pp.enumerate (Path.Build.Set.to_list targets) ~f:(fun p ->
              Pp.verbatim (Path.to_string_maybe_quoted (Path.build p)))
            ] );
      dir
  in
  { context; env; build; targets; mode; locks; info; dir }
