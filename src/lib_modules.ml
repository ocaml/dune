open Stdune

type t =
  { modules          : Module.Name_map.t
  ; virtual_modules  : Module.Name_map.t
  ; alias_module     : Module.t option
  ; main_module_name : Module.Name.t option
  ; wrapped_compat   : Module.Name_map.t
  }

let make_unwrapped ~modules ~virtual_modules ~main_module_name =
  assert (Module.Name.Map.is_empty virtual_modules);
  assert (main_module_name = None);
  { modules
  ; alias_module = None
  ; main_module_name = None
  ; wrapped_compat = Module.Name.Map.empty
  ; virtual_modules = Module.Name.Map.empty
  }

let make_wrapped ~(lib : Dune_file.Library.t) ~dir ~transition ~modules
      ~virtual_modules ~main_module_name =
  let wrap_modules modules =
    let open Module.Name.Infix in
    Module.Name.Map.map modules ~f:(fun (m : Module.t) ->
      if m.name = main_module_name then
        m
      else
        Module.with_wrapper m ~main_module_name)
  in
  let (modules, wrapped_compat) =
    if transition then
      ( wrap_modules modules
      , Module.Name.Map.remove modules main_module_name
        |> Module.Name.Map.filter_map ~f:(fun m ->
          if Module.is_public m then
            Some (Module.wrapped_compat m)
          else
            None)
      )
    else
      wrap_modules modules, Module.Name.Map.empty
  in
  let alias_module =
    let lib_name = Lib_name.Local.to_string (snd lib.name) in
    if Module.Name.Map.cardinal modules = 1 &&
       Module.Name.Map.mem modules main_module_name then
      None
    else if Module.Name.Map.mem modules main_module_name then
      (* This module needs an implementation for non-jbuilder
         users of the library:

         https://github.com/ocaml/dune/issues/567 *)
      Some
        (Module.make (Module.Name.add_suffix main_module_name "__")
           ~visibility:Public
           ~impl:(Module.File.make OCaml
                    (Path.relative dir (sprintf "%s__.ml-gen" lib_name)))
           ~obj_name:(lib_name ^ "__"))
    else
      Some
        (Module.make main_module_name
           ~visibility:Public
           ~impl:(Module.File.make OCaml
                    (Path.relative dir (lib_name ^ ".ml-gen")))
           ~obj_name:lib_name)
  in
  { modules
  ; alias_module
  ; main_module_name = Some main_module_name
  ; wrapped_compat
  ; virtual_modules
  }


let make (lib : Dune_file.Library.t) ~dir (modules : Module.Name_map.t)
      ~virtual_modules ~main_module_name =
  match lib.wrapped, main_module_name with
  | Simple false, _ ->
    make_unwrapped ~modules ~virtual_modules ~main_module_name
  | (Yes_with_transition _ | Simple true), None ->
    assert false
  | wrapped, Some main_module_name ->
    let transition =
      match wrapped with
      | Simple true -> false
      | Yes_with_transition _ -> true
      | Simple false -> assert false
    in
    make_wrapped ~transition ~modules ~virtual_modules ~dir ~main_module_name
      ~lib

module Alias_module = struct
  type t =
    { module_name : Module.Name.t
    ; alias_module : Module.t
    }
end

let alias t =
  match t.alias_module, t.main_module_name with
  | None, None -> None
  | None, Some _ -> None
  | Some _, None -> assert false
  | Some alias_module, Some module_name ->
    Some
      { Alias_module.
        module_name
      ; alias_module
      }

let installable_modules t =
  let modules =
    List.rev_append
      (Module.Name.Map.values t.modules)
      (Module.Name.Map.values t.wrapped_compat)
  in
  match t.alias_module with
  | None -> modules
  | Some alias -> alias :: modules

let lib_interface_module t =
  Option.bind t.main_module_name ~f:(Module.Name.Map.find t.modules)

let virtual_modules t = t.virtual_modules

let wrapped_compat t = t.wrapped_compat

let modules t = t.modules

let entry_modules t =
  match alias t with
  | None -> Module.Name.Map.values t.modules
  | Some { module_name ; alias_module } ->
    [Option.value ~default:alias_module
       (Module.Name.Map.find t.modules module_name)]
