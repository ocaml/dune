open! Stdune
open Import

module Info = struct
  type t =
    | From_dune_file of Loc.t
    | Internal
    | Source_file_copy

  let of_loc_opt = function
    | None -> Internal
    | Some loc -> From_dune_file loc
end

type t =
  { context  : Context.t option
  ; env      : Env.t option
  ; build    : (unit, Action.t) Build.t
  ; targets  : Path.Set.t
  ; sandbox  : bool
  ; mode     : Dune_file.Rule.Mode.t
  ; locks    : Path.t list
  ; info     : Info.t
  ; dir      : Path.t
  }

let make ?(sandbox=false) ?(mode=Dune_file.Rule.Mode.Standard)
      ~context ~env ?(locks=[]) ?(info=Info.Internal) build =
  let targets = Build.targets build in
  let dir =
    match Path.Set.choose targets with
    | None -> begin
        match info with
        | From_dune_file loc -> Errors.fail loc "Rule has no targets specified"
        | _ -> Exn.code_error "Build_interpret.Rule.make: no targets" []
      end
    | Some x ->
      let dir = Path.parent_exn x in
      if Path.Set.exists targets ~f:(fun path -> Path.parent_exn path <> dir)
      then begin
        match info with
        | Internal | Source_file_copy ->
          Exn.code_error "rule has targets in different directories"
            [ "targets", Path.Set.to_sexp targets
            ]
        | From_dune_file loc ->
          Errors.fail loc
            "Rule has targets in different directories.\nTargets:\n%s"
            (String.concat ~sep:"\n"
               (Path.Set.to_list targets |> List.map ~f:(fun p ->
                  sprintf "- %s"
                    (Path.to_string_maybe_quoted p))))
      end;
      dir
  in
  { context
  ; env
  ; build
  ; targets
  ; sandbox
  ; mode
  ; locks
  ; info
  ; dir
  }
