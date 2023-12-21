open Import

(* Expands a [String_with_vars.t] with a given function, returning the result
   unless the result is an absolute path in which case a user error is raised. *)
let expand_with_check_for_local_path ~expand sw =
  Memo.map (expand sw) ~f:(fun str ->
    if not (Filename.is_relative str)
    then (
      let loc = String_with_vars.loc sw in
      User_error.raise
        ~loc
        [ Pp.textf "Absolute paths are not allowed in the install stanza." ]);
    str)
;;

module Glob_files_with_optional_prefix = struct
  type t =
    { glob_files : Dep_conf.Glob_files.t
    ; prefix : String_with_vars.t option
    }

  let decode =
    let open Dune_lang.Decoder in
    let install_glob_version_check = Dune_lang.Syntax.since Stanza.syntax (3, 6) in
    let install_glob_with_prefix_version_check =
      Dune_lang.Syntax.since Stanza.syntax (3, 11)
    in
    let decode_args ~recursive =
      let decode_without_prefix =
        let+ glob = String_with_vars.decode in
        { glob_files = { Dep_conf.Glob_files.glob; recursive }; prefix = None }
      in
      let decode_with_prefix =
        enter
          (let* () = install_glob_with_prefix_version_check in
           let* glob = String_with_vars.decode in
           let* () = keyword "with_prefix" in
           let+ prefix = String_with_vars.decode in
           { glob_files = { Dep_conf.Glob_files.glob; recursive }; prefix = Some prefix })
      in
      decode_with_prefix <|> decode_without_prefix
    in
    sum
      [ "glob_files", install_glob_version_check >>> decode_args ~recursive:false
      ; "glob_files_rec", install_glob_version_check >>> decode_args ~recursive:true
      ]
  ;;
end

module File = struct
  module Without_include = struct
    type t =
      | File_binding of File_binding.Unexpanded.t
      | Glob_files of Glob_files_with_optional_prefix.t

    let decode =
      let open Dune_lang.Decoder in
      let file_binding_decode =
        let+ file_binding = File_binding.Unexpanded.decode in
        File_binding file_binding
      in
      let glob_files_decode =
        let+ glob_files = Glob_files_with_optional_prefix.decode in
        Glob_files glob_files
      in
      file_binding_decode <|> glob_files_decode
    ;;

    let to_file_bindings_unexpanded t ~expand ~dir ~dune_syntax =
      match t with
      | File_binding file_binding -> Memo.return [ file_binding ]
      | Glob_files { glob_files; prefix } ->
        let open Memo.O in
        let* glob_expanded = Glob_files_expand.memo glob_files ~f:expand ~base_dir:dir in
        let glob_loc = String_with_vars.loc glob_files.glob in
        let glob_prefix = Glob_files_expand.Expanded.prefix glob_expanded in
        let+ prefix_loc_opt =
          Memo.Option.map prefix ~f:(fun prefix_sw ->
            let loc = String_with_vars.loc prefix_sw in
            let+ prefix = expand prefix_sw >>| Value.to_string ~dir:(Path.build dir) in
            prefix, loc)
        in
        Glob_files_expand.Expanded.matches glob_expanded
        |> List.map ~f:(fun path ->
          let src = glob_loc, path in
          let dst =
            match prefix_loc_opt with
            | None -> src
            | Some (prefix, prefix_loc) ->
              let path_without_prefix =
                match String.drop_prefix path ~prefix:glob_prefix with
                | Some s -> s
                | None ->
                  Code_error.raise
                    ~loc:glob_loc
                    "Glob has a prefix which is not the same as the prefix of the match"
                    [ "glob_prefix", Dyn.string prefix; "match", Dyn.string path ]
              in
              let dst = Filename.concat prefix path_without_prefix in
              prefix_loc, dst
          in
          File_binding.Unexpanded.make
            ~src
            ~dst
            ~dune_syntax
            ~dir:(Some (Path.Build.drop_build_context_exn dir)))
    ;;

    let to_file_bindings_expanded t ~expand ~dir ~dune_syntax =
      to_file_bindings_unexpanded t ~expand ~dir ~dune_syntax
      |> Memo.bind
           ~f:
             (Memo.List.map
                ~f:
                  (File_binding.Unexpanded.expand
                     ~dir
                     ~f:
                       (expand_with_check_for_local_path ~expand:(fun p ->
                          expand p |> Memo.map ~f:(Value.to_string ~dir:(Path.build dir))))))
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

  let to_file_bindings_unexpanded ts ~expand ~dir =
    let open Memo.O in
    Memo.List.concat_map ts ~f:(fun { entry; dune_syntax } ->
      let+ with_include_expanded =
        Recursive_include.expand_include entry ~expand ~dir:(Path.build dir)
      in
      List.map with_include_expanded ~f:(fun entry -> entry, dune_syntax))
    |> Memo.bind
         ~f:
           (Memo.List.concat_map ~f:(fun (entry, dune_syntax) ->
              Without_include.to_file_bindings_unexpanded ~expand ~dir ~dune_syntax entry))
  ;;

  let to_file_bindings_expanded ts ~expand ~dir =
    let open Memo.O in
    let* file_bindings_expanded =
      Memo.List.concat_map ts ~f:(fun { entry; dune_syntax } ->
        let+ with_include_expanded =
          Recursive_include.expand_include entry ~expand ~dir:(Path.build dir)
        in
        List.map with_include_expanded ~f:(fun entry -> entry, dune_syntax))
      |> Memo.bind
           ~f:
             (Memo.List.concat_map ~f:(fun (entry, dune_syntax) ->
                Without_include.to_file_bindings_expanded ~expand ~dir ~dune_syntax entry))
    in
    (* Note that validation is deferred until after file bindings have been
       expanded as a path may be invalid due to the contents of a variable
       whose value is unknown until prior to this point. *)
    let+ () =
      Memo.parallel_iter
        file_bindings_expanded
        ~f:
          (File_binding.Expanded.validate_for_install_stanza
             ~relative_dst_path_starts_with_parent_error_when:
               `Deprecation_warning_from_3_11)
    in
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
    ~expand
    ~(dir : Path.Build.t)
    ~relative_dst_path_starts_with_parent_error_when
    =
    let open Memo.O in
    let* file_bindings_expanded =
      Memo.List.concat_map
        ts
        ~f:(Recursive_include.expand_include ~expand ~dir:(Path.build dir))
      >>= Memo.List.map
            ~f:
              (File_binding.Unexpanded.expand
                 ~dir
                 ~f:
                   (expand_with_check_for_local_path ~expand:(fun s ->
                      expand s >>| Value.to_string ~dir:(Path.build dir))))
    in
    (* Note that validation is deferred until after file bindings have been
       expanded as a path may be invalid due to the contents of a variable
       whose value is unknown until prior to this point. *)
    let+ () =
      Memo.parallel_iter
        file_bindings_expanded
        ~f:
          (File_binding.Expanded.validate_for_install_stanza
             ~relative_dst_path_starts_with_parent_error_when)
    in
    file_bindings_expanded
  ;;
end
