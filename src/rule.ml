open! Stdune
open Import

type t =
  { context  : Context.t option
  ; env      : Env.t option
  ; build    : (unit, Action.t) Build.t
  ; targets  : Path.Set.t
  ; sandbox  : bool
  ; mode     : Dune_file.Rule.Mode.t
  ; locks    : Path.t list
  ; loc      : Loc.t option
  ; dir      : Path.t
  }

let make ?(sandbox=false) ?(mode=Dune_file.Rule.Mode.Not_a_rule_stanza)
      ~context ~env ?(locks=[]) ?loc build =
  let targets = Build.targets build in
  let dir =
    match Path.Set.choose targets with
    | None -> begin
        match loc with
        | Some loc -> Errors.fail loc "Rule has no targets specified"
        | None -> Exn.code_error "Build_interpret.Rule.make: no targets" []
      end
    | Some x ->
      let dir = Path.parent_exn x in
      if Path.Set.exists targets ~f:(fun path -> Path.parent_exn path <> dir)
      then begin
        match loc with
        | None ->
          Exn.code_error "rule has targets in different directories"
            [ "targets", Path.Set.to_sexp targets
            ]
        | Some loc ->
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
  ; loc
  ; dir
  }
