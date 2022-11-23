open Import

module Modules_data = struct
  type t =
    { dir : Path.Build.t
    ; obj_dir : Path.Build.t Obj_dir.t
    ; sctx : Super_context.t
    ; vimpl : Vimpl.t option
    ; modules : Modules.t
    ; stdlib : Ocaml_stdlib.t option
    ; sandbox : Sandbox_config.t
    }
end

open Modules_data

let parse_module_names ~(unit : Module.t) ~modules words =
  List.filter_map words ~f:(fun m ->
      let m = Module_name.of_string m in
      Modules.find_dep modules ~of_:unit m)

let parse_deps_exn ~file lines =
  let invalid () =
    User_error.raise
      [ Pp.textf "ocamldep returned unexpected output for %s:" (* TODO: generalize *)
          (Path.to_string_maybe_quoted file)
      ; Pp.vbox
          (Pp.concat_map lines ~sep:Pp.cut ~f:(fun line ->
               Pp.seq (Pp.verbatim "> ") (Pp.verbatim line)))
      ]
  in
  match lines with
  | [] | _ :: _ :: _ -> invalid ()
  | [ line ] -> (
    match String.lsplit2 line ~on:':' with
    | None -> invalid ()
    | Some (basename, deps) ->
      let basename = Filename.basename basename in
      if basename <> Path.basename file then invalid ();
      String.extract_blank_separated_words deps)

let interpret_deps md ~unit deps =
  let dir = md.dir in
  let modules = md.modules in
  let deps = parse_module_names ~unit ~modules deps in
  if Option.is_none md.stdlib then
    Modules.main_module_name modules
    |> Option.iter ~f:(fun (main_module_name : Module_name.t) ->
           if
             Module_name.Infix.(Module.name unit <> main_module_name)
             && (not (Module.kind unit = Alias))
             && List.exists deps ~f:(fun x -> Module.name x = main_module_name)
           then
             User_error.raise
               [ Pp.textf "Module %s in directory %s depends on %s."
                   (Module_name.to_string (Module.name unit))
                   (Path.to_string_maybe_quoted (Path.build dir))
                   (Module_name.to_string main_module_name)
               ; Pp.textf "This doesn't make sense to me."
               ; Pp.nop
               ; Pp.textf
                   "%s is the main module of the library and is the only \
                    module exposed outside of the library. Consequently, it \
                    should be the one depending on all the other modules in \
                    the library."
                   (Module_name.to_string main_module_name)
               ]);
  match Modules.alias_for modules unit with
  | None -> deps
  | Some m -> m :: deps
