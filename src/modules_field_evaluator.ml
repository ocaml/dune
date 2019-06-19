open Stdune

module Buildable = Dune_file.Buildable

module Virtual = struct
  type t =
    { virtual_modules : Ordered_set_lang.t
    }
end

module Implementation = struct
  type t =
    { existing_virtual_modules : Module.Name.Set.t
    ; allow_new_public_modules : bool
    }
end

type kind =
  | Virtual of Virtual.t
  | Implementation of Implementation.t
  | Exe_or_normal_lib

let eval =
  let module Value = struct
    type t = (Module.Source.t, Module.Name.t) result

    type key = Module.Name.t

    let key = function
      | Error s -> s
      | Ok m -> Module.Source.name m
  end in
  let module Eval = Ordered_set_lang.Make_loc(Module.Name)(Value) in
  let parse ~all_modules ~fake_modules ~loc s =
    let name = Module.Name.of_string s in
    match Module.Name.Map.find all_modules name with
    | Some m -> Ok m
    | None ->
      fake_modules := Module.Name.Map.add !fake_modules name loc;
      Error name
  in
  fun ~fake_modules ~all_modules ~standard osl ->
    let parse = parse ~fake_modules ~all_modules in
    let standard = Module.Name.Map.map standard ~f:(fun m -> Ok m) in
    let modules = Eval.eval_unordered ~parse ~standard osl in
    Module.Name.Map.filter_map modules ~f:(fun (loc, m) ->
      match m with
      | Ok m -> Some (loc, m)
      | Error s ->
        (* We are going to fail only if the module appear in the final set,
           foo \ bar doesn't fail if bar doesn't exists (for jbuild file
           compatibility) *)
        Errors.fail loc "Module %a doesn't exist." Module.Name.pp s)

type single_module_error =
  | Spurious_module_intf
  | Spurious_module_virtual
  | Missing_intf_only
  | Virt_intf_overlap
  | Private_virt_module
  | Private_impl_of_vmodule
  | Vmodule_impl_intf_only_exclusion
  | Vmodule_impl_missing_impl
  | Forbidden_new_public_module
  | Vmodule_impls_with_own_intf

type errors =
  { errors : (single_module_error * Loc.t * Module.Name.t) list
  ; unimplemented_virt_modules : Module.Name.Set.t
  }

let find_errors ~modules ~intf_only ~virtual_modules ~private_modules
      ~existing_virtual_modules ~allow_new_public_modules =
  let all =
    (* We expect that [modules] is big and all the other ones are
       small, that's why the code is implemented this way. *)
    List.fold_left [intf_only; virtual_modules; private_modules]
      ~init:(Module.Name.Map.map modules ~f:snd)
      ~f:(fun acc map ->
        Module.Name.Map.foldi map ~init:acc ~f:(fun name (_loc, m) acc ->
          Module.Name.Map.add acc name m))
  in
  let errors =
    Module.Name.Map.foldi all ~init:[] ~f:(fun module_name module_ acc ->
      let has_impl = Module.Source.has module_ ~ml_kind:Impl in
      let has_intf = Module.Source.has module_ ~ml_kind:Intf in
      let impl_vmodule =
        Module.Name.Set.mem existing_virtual_modules module_name
      in
      let modules = Module.Name.Map.find modules module_name in
      let private_ = Module.Name.Map.find private_modules module_name in
      let virtual_ = Module.Name.Map.find virtual_modules module_name in
      let intf_only = Module.Name.Map.find intf_only module_name in
      let with_property prop f acc =
        match prop with
        | None -> acc
        | Some (loc, _) -> f loc acc
      in
      let add_if b kind loc acc =
        if b then
          (kind, loc, module_name) :: acc
        else
          acc
      in
      let (++) f g loc acc = f loc (g loc acc) in
      let (!?) = Option.is_some in
      with_property private_
        (add_if impl_vmodule Private_impl_of_vmodule)
      @@
      with_property intf_only
        (add_if has_impl Spurious_module_intf ++
         add_if impl_vmodule Vmodule_impl_intf_only_exclusion)
      @@
      with_property virtual_
        (add_if has_impl Spurious_module_virtual ++
         add_if (!? intf_only) Virt_intf_overlap ++
         add_if (!? private_) Private_virt_module)
      @@
      with_property modules
        (add_if (not (!? private_)
                 && not allow_new_public_modules
                 && not impl_vmodule)
           Forbidden_new_public_module ++
         add_if (not has_impl && not (!? intf_only) && not (!? virtual_))
           Missing_intf_only ++
         add_if (impl_vmodule && not has_impl)
           Vmodule_impl_missing_impl ++
         add_if (impl_vmodule && has_intf)
           Vmodule_impls_with_own_intf)
      @@
      acc)
  in
  let unimplemented_virt_modules =
    Module.Name.Set.filter existing_virtual_modules ~f:(fun module_name ->
      match Module.Name.Map.find all module_name with
      | None -> true
      | Some m -> not (Module.Source.has m ~ml_kind:Impl))
  in
  { errors
  ; unimplemented_virt_modules
  }

let check_invalid_module_listing ~(buildable : Buildable.t) ~intf_only
      ~modules ~virtual_modules ~private_modules ~existing_virtual_modules
      ~allow_new_public_modules =
  let { errors
      ; unimplemented_virt_modules
      } =
    find_errors ~modules ~intf_only ~virtual_modules ~private_modules
      ~existing_virtual_modules ~allow_new_public_modules
  in
  if not (List.is_empty errors) ||
     not (Module.Name.Set.is_empty unimplemented_virt_modules) then begin
    let get kind =
      List.filter_map errors ~f:(fun (k, loc, m) ->
        Option.some_if (kind = k) (loc, m))
      |> List.sort ~compare:(fun (_, a) (_, b) -> Module.Name.compare a b)
    in
    let vmodule_impls_with_own_intf = get Vmodule_impls_with_own_intf in
    let forbidden_new_public_modules = get Forbidden_new_public_module in
    let vmodule_impl_missing_impl = get Vmodule_impl_missing_impl in
    let vmodule_impl_intf_only_exclusion =
      get Vmodule_impl_intf_only_exclusion in
    let private_impl_of_vmodule = get Private_impl_of_vmodule in
    let private_virt_modules = get Private_virt_module in
    let virt_intf_overlaps = get Virt_intf_overlap in
    let missing_intf_only = get Missing_intf_only in
    let spurious_modules_intf = get Spurious_module_intf in
    let spurious_modules_virtual = get Spurious_module_virtual in
    let uncapitalized =
      List.map ~f:(fun (_, m) -> Module.Name.uncapitalize m) in
    let line_list modules =
      List.map ~f:(fun (_, m) ->
        m |> Module.Name.to_string |> sprintf "- %s") modules
      |> String.concat ~sep:"\n"
    in
    let print fmt l =
      match l with
      | [] -> ()
      | (loc, _) :: _ -> Errors.fail loc fmt (line_list l)
    in
    print "The folowing modules are implementations of virtual modules:\
           \n%s\nThey cannot have their own interface files."
      vmodule_impls_with_own_intf;
    print "Implementations of wrapped libraries cannot introduce new \
           public modules.\nThe following modules:\
           \n%s\n must all be marked as private using the \
           (private_modules ..) field."
      forbidden_new_public_modules;
    print "The following modules implement virtual modules but \
           do not have implementations:\
           \n%s\nYou must provide implementations for these"
      vmodule_impl_missing_impl;
    print "These modules are supposed to be implemented:\
           \n%s\nThey cannot be intferface only"
      vmodule_impl_intf_only_exclusion;
    print "These modules are virtual modules implementations:\
           \n%s\nThey cannot be private"
      private_impl_of_vmodule;
    print
      "The following modules are declared as virtual and private:\
       \n%s\nThis is not possible."
      private_virt_modules;
    print "These modules appear in the virtual_libraries \
           and modules_without_implementation fields:\
           \n%s\nThis is not possible."
      virt_intf_overlaps;
    print "These modules are declared virtual, but are missing.\
           \n%s\n\
           You must provide an implementation for all of these modules."
      (unimplemented_virt_modules
       |> Module.Name.Set.to_list
       |> List.map ~f:(fun name -> (buildable.loc, name)));
    if missing_intf_only <> [] then begin
      match Ordered_set_lang.loc buildable.modules_without_implementation with
      | None ->
        (* DUNE2: turn this into an error *)
        Errors.warn buildable.loc
          "Some modules don't have an implementation.\
           \nYou need to add the following field to this stanza:\
           \n\
           \n  %s\
           \n\
           \nThis will become an error in the future."
          (let tag =
             Dune_lang.unsafe_atom_of_string "modules_without_implementation" in
           let modules =
             missing_intf_only
             |> uncapitalized
             |> List.map ~f:Dune_lang.Encoder.string
           in
           Dune_lang.to_string ~syntax:Dune (List (tag :: modules)))
      | Some loc ->
        (* DUNE2: turn this into an error *)
        Errors.warn loc
          "The following modules must be listed here as they don't \
           have an implementation:\n\
           %s\n\
           This will become an error in the future."
          (line_list missing_intf_only)
    end;
    print
      "The following modules have an implementation, \
       they cannot be listed as modules_without_implementation:\n%s"
      spurious_modules_intf;
    print "The following modules have an implementation, they cannot \
           be listed as virtual:\n%s"
      spurious_modules_virtual
  end

let eval ~modules:(all_modules : Module.Source.t Module.Name.Map.t)
      ~buildable:(conf : Buildable.t) ~private_modules ~kind =
  (* fake modules are modules that doesn't exists but it doesn't
     matter because they are only removed from a set (for jbuild file
     compatibility) *)
  let fake_modules = ref Module.Name.Map.empty in
  let eval = eval ~fake_modules ~all_modules in
  let modules = eval ~standard:all_modules conf.modules in
  let intf_only =
    eval ~standard:Module.Name.Map.empty conf.modules_without_implementation
  in
  let allow_new_public_modules =
    match kind with
    | Exe_or_normal_lib
    | Virtual _ -> true
    | Implementation { allow_new_public_modules ; _ } ->
      allow_new_public_modules
  in
  let existing_virtual_modules =
    match kind with
    | Exe_or_normal_lib
    | Virtual _ -> Module.Name.Set.empty
    | Implementation { existing_virtual_modules; _ } -> existing_virtual_modules
  in
  let virtual_modules =
    match kind with
    | Exe_or_normal_lib
    | Implementation _ -> Module.Name.Map.empty
    | Virtual { virtual_modules } ->
      eval ~standard:Module.Name.Map.empty virtual_modules
  in
  let private_modules =
    eval ~standard:Module.Name.Map.empty private_modules
  in
  Module.Name.Map.iteri !fake_modules ~f:(fun m loc ->
    (* DUNE2: make this an error *)
    Errors.warn loc "Module %a is excluded but it doesn't exist."
      Module.Name.pp m
  );
  check_invalid_module_listing ~buildable:conf ~intf_only
    ~modules ~virtual_modules ~private_modules ~existing_virtual_modules
    ~allow_new_public_modules;
  let all_modules =
    Module.Name.Map.map modules ~f:(fun (_, m) ->
      let name = Module.Source.name m in
      let visibility =
        if Module.Name.Map.mem private_modules name then
          Visibility.Private
        else
          Public
      in
      let kind =
        if Module.Name.Map.mem virtual_modules name then
          Module.Kind.Virtual
        else if Module.Source.has m ~ml_kind:Impl then
          let name = Module.Source.name m in
          if Module.Name.Set.mem existing_virtual_modules name then
            Impl_vmodule
          else
            Impl
        else
          Intf_only
      in
      Module.of_source m ~kind ~visibility)
  in
  all_modules
