open Import

(* Expands a [String_with_vars.t] with a given function, returning the result
   unless the result is an absolute path in which case a user error is raised. *)
let expand_str_with_check_for_local_path ~expand_str sw =
  Memo.map (expand_str sw) ~f:(fun str ->
    if not (Filename.is_relative str)
    then (
      let loc = String_with_vars.loc sw in
      User_error.raise
        ~loc
        [ Pp.textf "Absolute paths are not allowed in the install stanza." ]);
    str)
;;

module File = struct
  module Without_include = struct
    type t =
      | File_binding of File_binding.Unexpanded.t
      | Glob_files of Dep_conf.Glob_files.t

    let decode =
      let open Dune_lang.Decoder in
      let file_binding_decode =
        let+ file_binding = File_binding.Unexpanded.decode in
        File_binding file_binding
      in
      let glob_files_decode =
        let version_check = Dune_lang.Syntax.since Stanza.syntax (3, 6) in
        let+ glob_files =
          sum
            [ ( "glob_files"
              , let+ glob = version_check >>> String_with_vars.decode in
                { Dep_conf.Glob_files.glob; recursive = false } )
            ; ( "glob_files_rec"
              , let+ glob = version_check >>> String_with_vars.decode in
                { Dep_conf.Glob_files.glob; recursive = true } )
            ]
        in
        Glob_files glob_files
      in
      file_binding_decode <|> glob_files_decode
    ;;

    let to_file_bindings_unexpanded t ~expand_str ~dir ~dune_syntax =
      match t with
      | File_binding file_binding -> Memo.return [ file_binding ]
      | Glob_files glob_files ->
        let open Memo.O in
        let+ paths = Glob_files_expand.memo glob_files ~f:expand_str ~base_dir:dir in
        let glob_loc = String_with_vars.loc glob_files.glob in
        List.map paths ~f:(fun path ->
          let src = glob_loc, path in
          File_binding.Unexpanded.make ~src ~dst:src ~dune_syntax)
    ;;

    let to_file_bindings_expanded t ~expand_str ~dir ~dune_syntax =
      to_file_bindings_unexpanded t ~expand_str ~dir ~dune_syntax
      |> Memo.bind
           ~f:
             (Memo.List.map
                ~f:
                  (File_binding.Unexpanded.expand
                     ~dir
                     ~f:(expand_str_with_check_for_local_path ~expand_str)))
    ;;
  end

  type t =
    { entry : Without_include.t Recursive_include.t
    ; dune_syntax : Syntax.Version.t
    }

  let decode =
    let open Dune_lang.Decoder in
    let+ entry =
      Recursive_include.decode
        ~base_term:Without_include.decode
        ~include_keyword:"include"
        ~non_sexp_behaviour:`User_error
        ~include_allowed_in_versions:(`Since (3, 5))
    and+ dune_syntax = Dune_lang.Syntax.get_exn Stanza.syntax in
    { entry; dune_syntax }
  ;;

  let of_file_binding file_binding =
    let entry = Recursive_include.of_base (Without_include.File_binding file_binding) in
    let dune_syntax = File_binding.Unexpanded.dune_syntax file_binding in
    { entry; dune_syntax }
  ;;

  let to_file_bindings_unexpanded ts ~expand_str ~dir =
    let open Memo.O in
    Memo.List.concat_map ts ~f:(fun { entry; dune_syntax } ->
      let+ with_include_expanded =
        Recursive_include.expand_include entry ~expand_str ~dir
      in
      List.map with_include_expanded ~f:(fun entry -> entry, dune_syntax))
    |> Memo.bind
         ~f:
           (Memo.List.concat_map ~f:(fun (entry, dune_syntax) ->
              Without_include.to_file_bindings_unexpanded
                ~expand_str
                ~dir
                ~dune_syntax
                entry))
  ;;

  let to_file_bindings_expanded ts ~expand_str ~dir =
    let open Memo.O in
    let+ file_bindings_expanded =
      Memo.List.concat_map ts ~f:(fun { entry; dune_syntax } ->
        let+ with_include_expanded =
          Recursive_include.expand_include entry ~expand_str ~dir
        in
        List.map with_include_expanded ~f:(fun entry -> entry, dune_syntax))
      |> Memo.bind
           ~f:
             (Memo.List.concat_map ~f:(fun (entry, dune_syntax) ->
                Without_include.to_file_bindings_expanded
                  ~expand_str
                  ~dir
                  ~dune_syntax
                  entry))
    in
    (* Note that validation is deferred until after file bindings have been
       expanded as a path may be invalid due to the contents of a variable
       whose value is unknown until prior to this point. *)
    List.iter
      file_bindings_expanded
      ~f:
        (File_binding.Expanded.validate_for_install_stanza
           ~relative_dst_path_starts_with_parent_error_when:`Deprecation_warning_from_3_11);
    file_bindings_expanded
  ;;
end

module Dir = struct
  let decode =
    Recursive_include.decode
      ~base_term:File_binding.Unexpanded.decode
      ~include_keyword:"include"
      ~non_sexp_behaviour:`User_error
      ~include_allowed_in_versions:(`Since (3, 5))
  ;;

  type t = File_binding.Unexpanded.t Recursive_include.t

  let to_file_bindings_expanded
    ts
    ~expand_str
    ~dir
    ~relative_dst_path_starts_with_parent_error_when
    =
    let open Memo.O in
    let+ file_bindings_expanded =
      Memo.List.concat_map ts ~f:(Recursive_include.expand_include ~expand_str ~dir)
      >>= Memo.List.map
            ~f:
              (File_binding.Unexpanded.expand
                 ~dir
                 ~f:(expand_str_with_check_for_local_path ~expand_str))
    in
    (* Note that validation is deferred until after file bindings have been
       expanded as a path may be invalid due to the contents of a variable
       whose value is unknown until prior to this point. *)
    List.iter
      file_bindings_expanded
      ~f:
        (File_binding.Expanded.validate_for_install_stanza
           ~relative_dst_path_starts_with_parent_error_when);
    file_bindings_expanded
  ;;
end
