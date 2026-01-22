open Import

type instance =
  { new_name : Module_name.t
  ; lib_name : Module_name.t
  ; args : (Loc.t * Module_name.t * Module_name.t) list
  ; loc : Loc.t
  }

type instances =
  | Simple of instance
  | Wrapped of Loc.t * Module_name.t * instance list

type t = instances list

let none : t = []

module Errors = struct
  let make_resolve ?loc ?hints paragraphs =
    Resolve.fail
      (User_error.make
         ?loc
         ?hints
         paragraphs
         ~annots:(User_message.Annots.singleton User_message.Annots.needs_stack_trace ()))
  ;;

  let make ?loc ?hints paragraphs = Memo.return @@ make_resolve ?loc ?hints paragraphs

  let library_not_found ~loc name =
    make ~loc [ Pp.textf "Library parameter %S not found." (Lib_name.to_string name) ]
  ;;

  let duplicate_parameters ~loc ~param arg arg' =
    make
      ~loc
      [ Pp.textf
          "Duplicate arguments %s and %s for parameter %s."
          (Lib_name.to_string (Lib.name arg))
          (Lib_name.to_string (Lib.name arg'))
          (Lib_name.to_string (Lib.name param))
      ]
  ;;

  let missing_implements ~loc p =
    let name = Lib_name.to_string (Lib.name p) in
    make ~loc [ Pp.textf "Library %S does not implement a library parameter." name ]
  ;;

  let unexpected_argument ?loc param arg =
    make
      ?loc
      [ Pp.textf
          "Argument %s implements unexpected parameter %s"
          (Lib_name.to_string (Lib.name arg))
          (Lib_name.to_string (Lib.name param))
      ]
      ~hints:[ Pp.text "Remove this argument" ]
  ;;

  let new_name_already_used ?loc name =
    make
      ?loc
      [ Pp.textf "The instance name %s is already used." (Module_name.to_string name) ]
  ;;

  let module_name_already_used ?loc name =
    make
      ?loc
      [ Pp.textf "Module name %s has already been used." (Module_name.to_string name) ]
  ;;
end

let for_ = Compilation_mode.Ocaml

let instances ~sctx ~db (deps : Lib_dep.t list) =
  let open Resolve.Memo.O in
  Resolve.Memo.List.concat_map deps ~f:(function
    | Lib_dep.Direct _ | Lib_dep.Re_export _ | Lib_dep.Select _ -> Resolve.Memo.return []
    | Lib_dep.Instantiate { loc; lib = lib_name; arguments; new_name } ->
      let* lib = Resolve.Memo.lift_memo @@ Lib.DB.find db lib_name in
      let lib =
        match lib with
        | None -> Code_error.raise "lib not found" [ "lib", Lib_name.to_dyn lib_name ]
        | Some lib -> lib
      in
      let* expected_params =
        let* parameters = Lib.parameters lib in
        let+ module_names =
          Resolve.Memo.List.filter_map parameters ~f:Lib.main_module_name
        in
        Module_name.Map.of_list_map_exn module_names ~f:(fun m -> m, [])
      in
      let+ entry_names = Root_module.entry_module_names sctx lib ~for_
      and+ args =
        Resolve.Memo.List.fold_left
          arguments
          ~init:expected_params
          ~f:(fun args (loc, arg_name) ->
            let* arg = Resolve.Memo.lift_memo @@ Lib.DB.find db arg_name in
            match arg with
            | None -> Errors.library_not_found ~loc arg_name
            | Some arg ->
              (match Lib.implements arg with
               | None -> Errors.missing_implements ~loc arg
               | Some param ->
                 let* param = param in
                 let* param_name = Lib.main_module_name param
                 and* arg_name = Lib.main_module_name arg in
                 (match param_name, arg_name with
                  | Some param_name, Some arg_name ->
                    (match Module_name.Map.find args param_name with
                     | Some [] ->
                       Resolve.Memo.return
                       @@ Module_name.Map.add_multi args param_name (loc, arg, arg_name)
                     | None -> Errors.unexpected_argument ~loc arg param
                     | Some ((_, existing, _) :: _) ->
                       Errors.duplicate_parameters ~loc ~param existing arg)
                  | None, None | Some _, None | None, Some _ ->
                    Errors.missing_implements ~loc arg)))
      in
      let args =
        Module_name.Map.foldi args ~init:[] ~f:(fun param arg_opt acc ->
          match arg_opt with
          | [] -> acc
          | [ (loc, _lib, arg) ] -> (loc, param, arg) :: acc
          | (_, arg, _) :: (_, arg', _) :: _ ->
            Code_error.raise
              "duplicate arguments were already reported"
              [ "arg", Lib.to_dyn arg; "arg'", Lib.to_dyn arg' ])
      in
      (match entry_names with
       | [] -> []
       | [ entry_name ] ->
         let new_name =
           match new_name with
           | None -> entry_name
           | Some new_name -> new_name
         in
         [ Simple { new_name; lib_name = entry_name; args; loc } ]
       | _ :: _ :: _ ->
         let instances =
           List.map entry_names ~f:(fun name ->
             { new_name = name; lib_name = name; args; loc })
         in
         (match new_name with
          | None -> List.map ~f:(fun i -> Simple i) instances
          | Some new_name -> [ Wrapped (loc, new_name, instances) ])))
;;

let check_instance known_names instance =
  if Module_name.Set.mem known_names instance.new_name
  then Errors.new_name_already_used ~loc:instance.loc instance.new_name
  else if Module_name.Set.mem known_names instance.lib_name
  then Errors.module_name_already_used ~loc:instance.loc instance.new_name
  else
    let open Resolve.Memo.O in
    let+ () =
      Resolve.Memo.List.iter instance.args ~f:(fun (loc, _param_name, arg_name) ->
        if Module_name.Set.mem known_names arg_name
        then Errors.module_name_already_used ~loc instance.new_name
        else Resolve.Memo.return ())
    in
    Module_name.Set.add known_names instance.new_name
;;

let check_instances instances =
  let open Resolve.Memo.O in
  Resolve.Memo.List.fold_left
    instances
    ~init:Module_name.Set.empty
    ~f:(fun acc -> function
    | Simple instance -> check_instance acc instance
    | Wrapped (loc, wrapped_name, instances) ->
      if Module_name.Set.mem acc wrapped_name
      then Errors.new_name_already_used ~loc wrapped_name
      else
        let+ _sub_definitions : Module_name.Set.t =
          Resolve.Memo.List.fold_left instances ~init:acc ~f:check_instance
        in
        Module_name.Set.add acc wrapped_name)
;;

let instances ~sctx ~db deps =
  let open Resolve.Memo.O in
  let* instances = instances ~sctx ~db deps in
  let+ (_ : Module_name.Set.t) = check_instances instances in
  instances
;;

let print_instance b indent instance =
  Printf.bprintf
    b
    "\n%smodule %s = %s%s [@jane.non_erasable.instances]"
    indent
    (Module_name.to_string instance.new_name)
    (Module_name.to_string instance.lib_name)
    (String.concat ~sep:""
     @@ List.map instance.args ~f:(fun (_loc, param_name, arg_name) ->
       Printf.sprintf
         "(%s)(%s)"
         (Module_name.to_string param_name)
         (Module_name.to_string arg_name)))
;;

let print_instances b instances =
  List.iter instances ~f:(fun instances ->
    match instances with
    | Simple instance -> print_instance b "" instance
    | Wrapped (_loc, new_name, instances) ->
      Printf.bprintf b "\nmodule %s = struct" (Module_name.to_string new_name);
      List.iter instances ~f:(print_instance b "  ");
      Printf.bprintf b "\nend\n")
;;
