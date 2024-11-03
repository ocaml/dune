open Import
open Memo.O

module Virtual = struct
  type t = { virtual_modules : Ordered_set_lang.Unexpanded.t }
end

module Implementation = struct
  type t =
    { existing_virtual_modules : Module_name.Path.Set.t
    ; allow_new_public_modules : bool
    }
end

type kind =
  | Virtual of Virtual.t
  | Implementation of Implementation.t
  | Exe_or_normal_lib

let eval0 =
  let key = function
    | Error s -> [ s ]
    | Ok m -> [ Module.Source.name m ]
  in
  let module Key = struct
    type t = Module_name.Path.t

    let compare = Module_name.Path.compare

    module Map = Module_trie
  end
  in
  let module Unordered = Ordered_set_lang.Unordered (Key) in
  (* Fake modules are modules that do not exist but it doesn't matter because
     they are only removed from a set (for jbuild file compatibility) *)
  let expand_and_eval t set ~parse ~key ~standard =
    let open Action_builder.O in
    let+ set = Expander.expand_ordered_set_lang t set in
    let fake_modules = ref Module_name.Map.empty in
    let r =
      let parse ~loc x = parse ~loc ~fake_modules x in
      Unordered.eval_loc set ~parse ~key ~standard
    in
    r, !fake_modules
  in
  let parse ~all_modules ~loc ~fake_modules s =
    let name = Module_name.of_string_allow_invalid (loc, s) in
    match Module_trie.find all_modules [ name ] with
    | Some m -> Ok m
    | None ->
      fake_modules := Module_name.Map.set !fake_modules name loc;
      Error name
  in
  fun ~expander ~loc ~all_modules ~standard osl ->
    let parse = parse ~all_modules in
    let standard = Module_trie.map standard ~f:(fun m -> loc, Ok m) in
    let+ (modules, fake_modules), _ =
      Action_builder.evaluate_and_collect_facts
        (expand_and_eval expander ~parse ~standard ~key osl)
    in
    let modules =
      Module_trie.filter_map modules ~f:(fun (loc, m) ->
        match m with
        | Ok m -> Some (loc, m)
        | Error s ->
          User_error.raise
            ~loc
            [ Pp.textf "Module %s doesn't exist." (Module_name.to_string s) ])
    in
    Module_name.Map.iteri
      ~f:(fun m loc ->
        User_error.raise
          ~loc
          [ Pp.textf
              "Module %s is excluded but it doesn't exist."
              (Module_name.to_string m)
          ])
      fake_modules;
    modules
;;

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
  | Undeclared_module_without_implementation
  | Undeclared_private_module
  | Undeclared_virtual_module

type errors =
  { errors : (single_module_error * Loc.t * Module_name.Path.t) list
  ; unimplemented_virt_modules : Module_name.Path.Set.t
  }

let find_errors
  ~modules
  ~intf_only
  ~virtual_modules
  ~private_modules
  ~existing_virtual_modules
  ~allow_new_public_modules
  =
  let all =
    (* We expect that [modules] is big and all the other ones are small, that's
       why the code is implemented this way. *)
    List.fold_left
      [ intf_only; virtual_modules; private_modules ]
      ~init:(Module_trie.map modules ~f:snd)
      ~f:(fun acc map ->
        Module_trie.foldi map ~init:acc ~f:(fun name (_loc, m) acc ->
          Module_trie.set acc name m))
  in
  let errors =
    Module_trie.foldi all ~init:[] ~f:(fun module_name module_ acc ->
      let has_impl = Module.Source.has module_ ~ml_kind:Impl in
      let has_intf = Module.Source.has module_ ~ml_kind:Intf in
      let impl_vmodule = Module_name.Path.Set.mem existing_virtual_modules module_name in
      let modules = Module_trie.find modules module_name in
      let private_ = Module_trie.find private_modules module_name in
      let virtual_ = Module_trie.find virtual_modules module_name in
      let intf_only = Module_trie.find intf_only module_name in
      let with_property prop f acc =
        match prop with
        | None -> acc
        | Some (loc, _) -> f loc acc
      in
      let add_if b kind loc acc = if b then (kind, loc, module_name) :: acc else acc in
      let ( ++ ) f g loc acc = f loc (g loc acc) in
      let ( !? ) = Option.is_some in
      with_property
        private_
        (add_if impl_vmodule Private_impl_of_vmodule
         ++ add_if (not !?modules) Undeclared_private_module)
      @@ with_property
           intf_only
           (add_if has_impl Spurious_module_intf
            ++ add_if impl_vmodule Vmodule_impl_intf_only_exclusion
            ++ add_if (not !?modules) Undeclared_module_without_implementation)
      @@ with_property
           virtual_
           (add_if has_impl Spurious_module_virtual
            ++ add_if !?intf_only Virt_intf_overlap
            ++ add_if !?private_ Private_virt_module
            ++ add_if (not !?modules) Undeclared_virtual_module)
      @@ with_property
           modules
           (add_if
              ((not !?private_) && (not allow_new_public_modules) && not impl_vmodule)
              Forbidden_new_public_module
            ++ add_if
                 ((not has_impl) && (not !?intf_only) && not !?virtual_)
                 Missing_intf_only
            ++ add_if (impl_vmodule && not has_impl) Vmodule_impl_missing_impl
            ++ add_if (impl_vmodule && has_intf) Vmodule_impls_with_own_intf)
      @@ acc)
  in
  let unimplemented_virt_modules =
    Module_name.Path.Set.filter existing_virtual_modules ~f:(fun module_name ->
      match Module_trie.find all module_name with
      | None -> true
      | Some m -> not (Module.Source.has m ~ml_kind:Impl))
  in
  { errors; unimplemented_virt_modules }
;;

let check_invalid_module_listing
  ~stanza_loc
  ~modules_without_implementation
  ~intf_only
  ~modules
  ~virtual_modules
  ~private_modules
  ~existing_virtual_modules
  ~allow_new_public_modules
  ~is_vendored
  ~version
  =
  let { errors; unimplemented_virt_modules } =
    find_errors
      ~modules
      ~intf_only
      ~virtual_modules
      ~private_modules
      ~existing_virtual_modules
      ~allow_new_public_modules
  in
  if List.is_non_empty errors
     || not (Module_name.Path.Set.is_empty unimplemented_virt_modules)
  then (
    let get kind =
      List.filter_map errors ~f:(fun (k, loc, m) -> Option.some_if (kind = k) (loc, m))
      |> List.sort ~compare:(fun (_, a) (_, b) -> Module_name.Path.compare a b)
    in
    let vmodule_impls_with_own_intf = get Vmodule_impls_with_own_intf in
    let forbidden_new_public_modules = get Forbidden_new_public_module in
    let vmodule_impl_missing_impl = get Vmodule_impl_missing_impl in
    let vmodule_impl_intf_only_exclusion = get Vmodule_impl_intf_only_exclusion in
    let private_impl_of_vmodule = get Private_impl_of_vmodule in
    let private_virt_modules = get Private_virt_module in
    let virt_intf_overlaps = get Virt_intf_overlap in
    let missing_intf_only = get Missing_intf_only in
    let spurious_modules_intf = get Spurious_module_intf in
    let spurious_modules_virtual = get Spurious_module_virtual in
    let undeclared_modules_without_implementation =
      get Undeclared_module_without_implementation
    in
    let undeclared_private_modules = get Undeclared_private_module in
    let undeclared_virtual_modules = get Undeclared_virtual_module in
    let uncapitalized = List.map ~f:(fun (_, m) -> Module_name.Path.uncapitalize m) in
    let line_list modules =
      Pp.enumerate modules ~f:(fun (_, m) -> Pp.verbatim (Module_name.Path.to_string m))
    in
    let print ?(is_error = true) before l after =
      match l with
      | [] -> ()
      | (loc, _) :: _ ->
        User_warning.emit ~is_error ~loc (List.concat [ before; [ line_list l ]; after ])
    in
    print
      [ Pp.text "The following modules are implementations of virtual modules:" ]
      vmodule_impls_with_own_intf
      [ Pp.text "They cannot have their own interface files." ];
    print
      [ Pp.text
          "Implementations of wrapped libraries cannot introduce new public modules."
      ; Pp.text "The following modules:"
      ]
      forbidden_new_public_modules
      [ Pp.text "must all be marked as private using the (private_modules ..) field." ];
    print
      [ Pp.text
          "The following modules implement virtual modules but do not have \
           implementations:"
      ]
      vmodule_impl_missing_impl
      [ Pp.text "You must provide implementations for these." ];
    print
      [ Pp.text "These modules are supposed to be implemented:" ]
      vmodule_impl_intf_only_exclusion
      [ Pp.text "They cannot be interface only" ];
    print
      [ Pp.text "These modules are virtual modules implementations:" ]
      private_impl_of_vmodule
      [ Pp.text "They cannot be private." ];
    print
      [ Pp.text "The following modules are declared as virtual and private:" ]
      private_virt_modules
      [ Pp.text "This is not possible." ];
    print
      [ Pp.text
          "These modules appear in the virtual_libraries and \
           modules_without_implementation fields:"
      ]
      virt_intf_overlaps
      [ Pp.text "This is not possible." ];
    print
      [ Pp.text "These modules are declared virtual, but are missing." ]
      (unimplemented_virt_modules
       |> Module_name.Path.Set.to_list
       |> List.map ~f:(fun name -> stanza_loc, name))
      [ Pp.text "You must provide an implementation for all of these modules." ];
    (* Checking that (modules) includes all declared modules *)
    let print_undelared_modules field mods =
      (* If we are in a vendored stanza we do nothing. *)
      if not is_vendored
      then
        print
          ~is_error:(version >= (3, 11))
          [ Pp.textf "These modules appear in the %s field:" field ]
          mods
          [ Pp.text "They must also appear in the modules field." ]
    in
    print_undelared_modules
      "modules_without_implementation"
      undeclared_modules_without_implementation;
    print_undelared_modules "private_modules" undeclared_private_modules;
    print_undelared_modules "virtual_modules" undeclared_virtual_modules;
    if missing_intf_only <> []
    then (
      match Ordered_set_lang.Unexpanded.loc modules_without_implementation with
      | None ->
        User_error.raise
          ~loc:stanza_loc
          [ Pp.text "Some modules don't have an implementation."
          ; Pp.text "You need to add the following field to this stanza:"
          ; Pp.nop
          ; Pp.textf
              "  %s"
              (let tag = Dune_lang.atom "modules_without_implementation" in
               let modules =
                 missing_intf_only
                 |> uncapitalized
                 |> List.map ~f:Dune_lang.Encoder.string
               in
               Dune_lang.to_string (List (tag :: modules)))
          ]
      | Some loc ->
        User_error.raise
          ~loc
          [ Pp.text
              "The following modules must be listed here as they don't have an \
               implementation:"
          ; line_list missing_intf_only
          ]);
    print
      [ Pp.text
          "The following modules have an implementation, they cannot be listed as \
           modules_without_implementation:"
      ]
      spurious_modules_intf
      [];
    print
      [ Pp.text
          "The following modules have an implementation, they cannot be listed as \
           virtual:"
      ]
      spurious_modules_virtual
      [])
;;

let eval
  ~expander
  ~modules:(all_modules : Module.Source.t Module_trie.t)
  ~stanza_loc
  ~private_modules
  ~kind
  ~src_dir
  ~is_vendored
  ~version
  { Stanza_common.Modules_settings.modules = _
  ; root_module
  ; modules_without_implementation
  }
  modules
  =
  (* Fake modules are modules that do not exist but it doesn't matter because
     they are only removed from a set (for jbuild file compatibility) *)
  let eval = eval0 ~expander ~loc:stanza_loc ~all_modules in
  let allow_new_public_modules =
    match kind with
    | Exe_or_normal_lib | Virtual _ -> true
    | Implementation { allow_new_public_modules; _ } -> allow_new_public_modules
  in
  let existing_virtual_modules =
    match kind with
    | Exe_or_normal_lib | Virtual _ -> Module_name.Path.Set.empty
    | Implementation { existing_virtual_modules; _ } -> existing_virtual_modules
  in
  let+ intf_only = eval ~standard:Module_trie.empty modules_without_implementation
  and+ virtual_modules =
    match kind with
    | Exe_or_normal_lib | Implementation _ -> Memo.return Module_trie.empty
    | Virtual { virtual_modules } -> eval ~standard:Module_trie.empty virtual_modules
  and+ private_modules = eval ~standard:Module_trie.empty private_modules in
  check_invalid_module_listing
    ~stanza_loc
    ~modules_without_implementation
    ~intf_only
    ~modules
    ~virtual_modules
    ~private_modules
    ~existing_virtual_modules
    ~allow_new_public_modules
    ~is_vendored
    ~version;
  let all_modules =
    Module_trie.mapi modules ~f:(fun _path (_, m) ->
      let name = [ Module.Source.name m ] in
      let visibility =
        if Module_trie.mem private_modules name then Visibility.Private else Public
      in
      let kind =
        if Module_trie.mem virtual_modules name
        then Module.Kind.Virtual
        else if Module.Source.has m ~ml_kind:Impl
        then (
          let name = Module.Source.name m in
          if Module_name.Path.Set.mem existing_virtual_modules [ name ]
          then Impl_vmodule
          else Impl)
        else Intf_only
      in
      Module.of_source m ~kind ~visibility)
  in
  match root_module with
  | None -> all_modules
  | Some (_, name) ->
    let path = [ name ] in
    let module_ = Module.generated ~kind:Root ~src_dir path in
    Module_trie.set all_modules path module_
;;

let eval
  ~expander
  ~modules:(all_modules : Module.Source.t Module_trie.t)
  ~stanza_loc
  ~private_modules
  ~kind
  ~src_dir
  ~version
  (settings : Stanza_common.Modules_settings.t)
  =
  Memo.push_stack_frame ~human_readable_description:(fun () ->
    Pp.textf "(modules) field at %s" (Loc.to_file_colon_line stanza_loc))
  @@ fun () ->
  let* modules0 =
    eval0 ~expander ~loc:stanza_loc ~all_modules ~standard:all_modules settings.modules
  in
  let* is_vendored =
    match Path.Build.drop_build_context src_dir with
    | Some src_dir -> Source_tree.is_vendored src_dir
    | None -> Memo.return false
  in
  let+ modules =
    eval
      ~expander
      ~modules:all_modules
      ~stanza_loc
      ~private_modules
      ~kind
      ~src_dir
      ~is_vendored
      settings
      modules0
      ~version
  in
  modules0, modules
;;
