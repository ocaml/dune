open Stdune

type t =
  { modules          : Module.Name_map.t
  ; virtual_modules  : Module.Name_map.t
  ; alias_module     : Module.t option
  ; main_module_name : Module.Name.t option
  ; wrapped_compat   : Module.Name_map.t
  ; implements       : bool
  }

let virtual_modules t = t.virtual_modules
let alias_module t = t.alias_module
let wrapped_compat t = t.wrapped_compat
let modules t = t.modules
let main_module_name t = t.main_module_name

let make_unwrapped ~modules ~virtual_modules ~main_module_name =
  assert (Module.Name.Map.is_empty virtual_modules);
  assert (main_module_name = None);
  { modules
  ; alias_module = None
  ; main_module_name = None
  ; wrapped_compat = Module.Name.Map.empty
  ; virtual_modules = Module.Name.Map.empty
  ; implements = false
  }

let make_alias_module ~dir ~implements ~lib_name ~stdlib
      ~main_module_name ~modules =
  let alias_prefix =
    String.uncapitalize (Module.Name.to_string main_module_name) in
  if implements then
    let alias_prefix =
      sprintf "%s__%s__" alias_prefix
        (Lib_name.Local.to_string lib_name) in
    let name = Module.Name.of_string alias_prefix in
    Some
      (Module.make name
         ~visibility:Public
         ~impl:(Module.File.make OCaml
                  (Path.relative dir (sprintf "%s.ml-gen" alias_prefix)))
         ~obj_name:alias_prefix)
  else if Module.Name.Map.cardinal modules = 1 &&
          Module.Name.Map.mem modules main_module_name ||
          stdlib then
    None
  else if Module.Name.Map.mem modules main_module_name then
    (* This module needs an implementation for non-dune
       users of the library:

       https://github.com/ocaml/dune/issues/567 *)
    Some
      (Module.make (Module.Name.add_suffix main_module_name "__")
         ~visibility:Public
         ~impl:(Module.File.make OCaml
                  (Path.relative dir (sprintf "%s__.ml-gen" alias_prefix)))
         ~obj_name:(alias_prefix ^ "__"))
  else
    Some
      (Module.make main_module_name
         ~visibility:Public
         ~impl:(Module.File.make OCaml
                  (Path.relative dir (alias_prefix ^ ".ml-gen")))
         ~obj_name:alias_prefix)

let make_alias_module_of_lib ~dir ~lib ~main_module_name ~modules =
  make_alias_module ~dir ~main_module_name
    ~modules
    ~implements:(Dune_file.Library.is_impl lib)
    ~lib_name:(snd lib.name)
    ~stdlib:(Option.is_some lib.stdlib)

let wrap_modules ~modules ~lib ~main_module_name =
  let open Module.Name.Infix in
  let prefix =
    if not (Dune_file.Library.is_impl lib) then
      fun _ -> main_module_name
    else
      (* for implementations we need to pick a different prefix for private
         modules. This is to guarantee that the private modules will never
         collide with the names of modules in the virtual library. *)
      let private_module_prefix =
        if Dune_file.Library.is_impl lib then
          Module.Name.of_string
            (sprintf "%s__%s"
               (Module.Name.to_string main_module_name)
               (Lib_name.Local.to_string (snd lib.name)))
        else
          main_module_name
      in
      fun m ->
        if Module.is_private m then
          private_module_prefix
        else
          main_module_name
  in
  Module.Name.Map.map modules ~f:(fun (m : Module.t) ->
    if Module.name m = main_module_name ||
       Dune_file.Library.special_compiler_module lib m then
      m
    else
      Module.with_wrapper m ~main_module_name:(prefix m))

let make_wrapped ~(lib : Dune_file.Library.t) ~dir ~transition ~modules
      ~virtual_modules ~main_module_name =
  let (modules, wrapped_compat) =
    if transition then
      ( wrap_modules ~modules ~main_module_name ~lib
      , Module.Name.Map.remove modules main_module_name
        |> Module.Name.Map.filter_map ~f:(fun m ->
          if Module.is_public m then
            Some (Module.wrapped_compat m)
          else
            None)
      )
    else
      (wrap_modules ~modules ~main_module_name ~lib, Module.Name.Map.empty)
  in
  let alias_module =
    make_alias_module_of_lib ~main_module_name ~dir ~lib ~modules
  in
  { modules
  ; alias_module
  ; main_module_name = Some main_module_name
  ; wrapped_compat
  ; virtual_modules
  ; implements = Dune_file.Library.is_impl lib
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
    { main_module_name : Module.Name.t
    ; alias_module : Module.t
    }
end

let alias t =
  match t.alias_module, t.main_module_name with
  | None, None -> None
  | None, Some _ -> None
  | Some _, None -> assert false
  | Some alias_module, Some main_module_name ->
    Some
      { Alias_module.
        main_module_name
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
  if t.implements then
    None
  else
    match t.main_module_name, t.alias_module with
    | None, None -> None
    | None, Some _ -> assert false
    | Some main_module_name, None ->
      Module.Name.Map.find t.modules main_module_name
    | Some main_module_name, Some alias_module ->
      Module.Name.Map.find t.modules main_module_name
      |> Option.value ~default:alias_module
      |> Option.some

let entry_modules t =
  match lib_interface_module t with
  | None -> Module.Name.Map.values t.modules
  | Some m -> [m]

let set_modules t pped_modules =
  { t with modules = pped_modules }

let for_compilation t =
  match t.alias_module with
  | None -> t.modules
  | Some alias -> Module.Name_map.add t.modules alias

let has_private_modules t =
  Module.Name.Map.exists t.modules ~f:Module.is_private

let public_modules t =
  Module.Name.Map.filter ~f:Module.is_public t.modules

let have_artifacts t =
  let base =
    Module.Name.Map.superpose t.modules t.wrapped_compat in
  match t.alias_module with
  | None -> base
  | Some alias_module -> Module.Name_map.add base alias_module

module Virtual = struct
  module M = struct
    module Kind = struct
      type t =
        | Intf_only
        | Virtual
        | Private
        | Public

      let encode =
        let open Dune_lang.Encoder in
        function
        | Intf_only -> string "intf_only"
        | Virtual -> string "virtual"
        | Private -> string "private"
        | Public -> string "public"

      let decode =
        let open Stanza.Decoder in
        enum
          [ "intf_only", Intf_only
          ; "virtual", Virtual
          ; "private", Private
          ; "public", Public
          ]

      let of_module ~virtual_modules m =
        match Module.Name.Map.mem virtual_modules (Module.name m)
            , Module.is_private m
            , Module.has_impl m with
        | true, false, false -> Some Virtual
        | false, false, true -> Some Public
        | false, false, false -> Some Intf_only
        | false, true, true -> Some Private
        | false, true, false -> None (* we don't need these modules *)
        | true, true, _ (* no private virtual modules*)
        | true, false, true (* virtual modules don't have impls *)
          -> assert false

      let has_impl = function
        | Private
        | Public -> true
        | Intf_only
        | Virtual -> false

      let has_intf = function
        | Private -> false
        | Public
        | Intf_only
        | Virtual -> true

      let visibility = function
        | Private -> Module.Visibility.Private
        | Virtual
        | Intf_only
        | Public -> Module.Visibility.Public

      let is_virtual = function
        | Virtual -> true
        | _ -> false
    end
    type t =
      { name : Module.Name.t
      ; kind : Kind.t
      }

    let encode { name ; kind } =
      let open Dune_lang.Encoder in
      list (fun x -> x) (
        record_fields Dune
          [ field "name" Module.Name.encode name
          ; field "kind" Kind.encode kind
          ])

    let decode =
      let open Stanza.Decoder in
      enter @@ record (
        let%map name = field "name" Module.Name.decode
        and kind = field "kind" Kind.decode
        in
        { name
        ; kind
        }
      )
  end

  let encode t =
    Module.Name.Map.values t.modules
    |> List.filter_map ~f:(fun m ->
      M.Kind.of_module ~virtual_modules:t.virtual_modules m
      |> Option.map ~f:(fun kind ->
        let name = Module.name m in
        { M. name ; kind }
        |> M.encode))

  let decode ~main_module_name ~dir =
    let file ext m =
      Module.File.make Module.Syntax.OCaml
        (Path.L.relative dir [Module.Name.to_string m; ext])
    in
    let impl = file ".ml" in
    let intf = file ".mli" in
    let open Stanza.Decoder in
    repeat (located M.decode) >>| fun ms ->
    let modules =
      Module.Name.Map.of_list_map ms
        ~f:(fun (loc, { M. kind ; name }) ->
          let intf = if M.Kind.has_intf kind then Some (intf name) else None in
          let impl = if M.Kind.has_impl kind then Some (impl name) else None in
          let visibility = M.Kind.visibility kind in
          let module_ = Module.make name ~visibility ?intf ?impl in
          (name, (loc, module_)))
      |> (function
        | Result.Ok m -> m
        | Error (name, (_, _), (loc, _)) ->
          Errors.fail loc "module named %a is listed twice" Module.Name.pp name)
      |> Module.Name.Map.map ~f:snd
    in
    let virtual_modules =
      Module.Name.Map.filter modules ~f:(fun m ->
        List.exists ~f:(fun (_loc, (m' : M.t)) ->
          Module.name m = m'.name && M.Kind.is_virtual m'.kind) ms
      ) in
    let alias_module =
      let lib_name = Module.Name.to_local_lib_name main_module_name in
      make_alias_module ~dir ~main_module_name ~modules
        ~stdlib:false ~implements:false ~lib_name
    in
    { modules
    ; virtual_modules
    ; alias_module
    ; main_module_name = Some main_module_name
    ; wrapped_compat = Module.Name.Map.empty
    ; implements = false
    }
end

let encode
      { modules
      ; virtual_modules
      ; alias_module
      ; main_module_name
      ; wrapped_compat = _
      ; implements = _
      } =
  let open Dune_lang.Encoder in
  record_fields Dune
    [ field_l "alias_module" (fun x -> x)
        (match alias_module with
         | None -> []
         | Some m -> Module.encode m)
    ; field_o "main_module_name" Module.Name.encode main_module_name
    ; field_l "modules" (fun x -> Dune_lang.List (Module.encode x))
        (Module.Name.Map.values modules)
    ; field_l "virtual_modules" Module.Name.encode
        (Module.Name.Map.keys virtual_modules)
    ]

let decode ~implements ~dir =
  let open Stanza.Decoder in
  fields (
    let%map alias_module = field_o "alias_module" (Module.decode ~dir)
    and main_module_name = field_o "main_module_name" Module.Name.decode
    and modules =
      field ~default:[] "modules" (list (enter (Module.decode ~dir)))
    and virtual_modules =
      field ~default:[] "virtual_modules" (list Module.Name.decode)
    in
    let modules =
      modules
      |> List.map ~f:(fun m -> (Module.name m, m))
      |> Module.Name.Map.of_list_exn
    in
    let virtual_modules =
      List.map virtual_modules ~f:(fun m ->
        (m, Module.Name.Map.find_exn modules m))
      |> Module.Name.Map.of_list_exn
    in
    { modules
    ; virtual_modules
    ; alias_module
    ; implements
    ; wrapped_compat = Module.Name.Map.empty
    ; main_module_name
    }
  )
