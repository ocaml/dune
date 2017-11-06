open Import
open Build.O

module SC = Super_context

type dep_graph = (unit, string list String_map.t) Build.t Ml_kind.Dict.t

let parse_deps ~dir lines ~modules ~alias_module ~lib_interface_module =
  List.map lines ~f:(fun line ->
    match String.index line ':' with
    | None -> die "`ocamldep` in %s returned invalid line: %S" (Path.to_string dir) line
    | Some i ->
      let unit =
        let basename =
          String.sub line ~pos:0 ~len:i
          |> Filename.basename
        in
        let module_basename =
          match String.index basename '.' with
          | None -> basename
          | Some i -> String.sub basename ~pos:0 ~len:i
        in
        String.capitalize_ascii module_basename
      in
      let deps =
        String.extract_blank_separated_words (String.sub line ~pos:(i + 1)
                                                ~len:(String.length line - (i + 1)))
        |> List.filter ~f:(fun m -> m <> unit && String_map.mem m modules)
      in
      (match lib_interface_module with
       | None -> ()
       | Some (m : Module.t) ->
         let is_alias_module =
           match alias_module with
           | None -> false
           | Some (m : Module.t) -> unit = m.name
         in
         if unit <> m.name && not is_alias_module && List.mem m.name ~set:deps then
           die "Module %s in directory %s depends on %s.\n\
                This doesn't make sense to me.\n\
                \n\
                %s is the main module of the library and is the only module exposed \n\
                outside of the library. Consequently, it should be the one depending \n\
                on all the other modules in the library."
             unit (Path.to_string dir) m.name m.name);
      let deps =
        match alias_module with
        | None -> deps
        | Some (m : Module.t) -> m.name :: deps
      in
      (unit, deps))
  |> String_map.of_alist
  |> function
  | Ok x -> begin
      match alias_module with
      | None -> x
      | Some m -> String_map.add x ~key:m.name ~data:[]
    end
  | Error (unit, _, _) ->
    die
      "`ocamldep` in %s returned %s several times" (Path.to_string dir) unit

let rules sctx ~ml_kind ~dir ~item ~modules ~alias_module ~lib_interface_module =
  let suffix = Ml_kind.suffix ml_kind in
  let files =
    List.filter_map (String_map.values modules) ~f:(fun m -> Module.file ~dir m ml_kind)
    |> List.map ~f:(fun fn ->
      match ml_kind, Filename.extension (Path.to_string fn) with
      | Impl, ".ml"  -> Arg_spec.Dep fn
      | Intf, ".mli" -> Dep fn
      | Impl, _ -> S [A "-impl"; Dep fn]
      | Intf, _ -> S [A "-intf"; Dep fn])
  in
  let ocamldep_output =
    Path.relative dir (sprintf "%s.depends%s.ocamldep-output" item suffix)
  in
  let ctx = SC.context sctx in
  SC.add_rule sctx
    (Build.run ~context:ctx (Ok ctx.ocamldep) [A "-modules"; S files]
       ~stdout_to:ocamldep_output);
  Build.memoize (Path.to_string ocamldep_output)
    (Build.lines_of ocamldep_output
     >>^ parse_deps ~dir ~modules ~alias_module ~lib_interface_module)

module Dep_closure =
  Top_closure.Make(String)(struct
    type t = string
    type graph = Path.t * t list String_map.t
    let key t = t
    let deps t (dir, map) = Utils.find_deps ~dir map t
  end)

let dep_closure ~dir dep_graph names =
  match Dep_closure.top_closure (dir, dep_graph) names with
  | Ok names -> names
  | Error cycle ->
    die "dependency cycle between modules in %s:\n   %s" (Path.to_string dir)
      (String.concat cycle ~sep:"\n-> ")

let names_to_top_closed_cm_files ~dir ~dep_graph ~modules ~mode names =
  let cm_kind = Mode.cm_kind mode in
  List.map (dep_closure ~dir dep_graph names) ~f:(fun name ->
    let m = Utils.find_module ~dir modules name in
    Module.cm_file m ~dir cm_kind)

let rules sctx ~dir ~item ~modules ~alias_module ~lib_interface_module =
  Ml_kind.Dict.of_func (rules sctx ~dir ~item ~modules ~alias_module ~lib_interface_module)
