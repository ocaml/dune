open Import

(* Expands a [String_with_vars.t] with a given function, returning the result
   unless the result is an absolute path in which case a user error is raised. *)
let expand_str_with_check_for_local_path ~expand_str sw =
  Memo.map (expand_str sw) ~f:(fun str ->
      (if not (Filename.is_relative str) then
       let loc = String_with_vars.loc sw in
       User_error.raise ~loc
         [ Pp.textf "Absolute paths are not allowed in the install stanza." ]);
      str)

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

    let to_file_bindings_unexpanded t ~expand_str ~dir =
      match t with
      | File_binding file_binding -> Memo.return [ file_binding ]
      | Glob_files glob_files ->
        let open Memo.O in
        let+ paths =
          Glob_files_expand.memo glob_files ~f:expand_str ~base_dir:dir
        in
        let glob_loc = String_with_vars.loc glob_files.glob in
        List.map paths ~f:(fun path ->
            let src = (glob_loc, path) in
            File_binding.Unexpanded.make ~src ~dst:src)

    let to_file_bindings_expanded t ~expand_str ~dir =
      to_file_bindings_unexpanded t ~expand_str ~dir
      |> Memo.bind
           ~f:
             (Memo.List.map
                ~f:
                  (File_binding.Unexpanded.expand ~dir
                     ~f:(expand_str_with_check_for_local_path ~expand_str)))
  end

  include
    Recursive_include.Make
      (Without_include)
      (struct
        let include_keyword = "include"

        let include_allowed_in_versions = `Since (3, 5)

        let non_sexp_behaviour = `User_error
      end)

  let expand_include_multi ts ~expand_str ~dir =
    Memo.List.concat_map ts ~f:(expand_include ~expand_str ~dir)

  let of_file_binding file_binding =
    of_base (Without_include.File_binding file_binding)

  let to_file_bindings_unexpanded ts ~expand_str ~dir =
    expand_include_multi ts ~expand_str ~dir
    |> Memo.bind
         ~f:
           (Memo.List.concat_map
              ~f:(Without_include.to_file_bindings_unexpanded ~expand_str ~dir))

  let to_file_bindings_expanded ts ~expand_str ~dir =
    expand_include_multi ts ~expand_str ~dir
    |> Memo.bind
         ~f:
           (Memo.List.concat_map
              ~f:(Without_include.to_file_bindings_expanded ~expand_str ~dir))
end

module Dir = struct
  include
    Recursive_include.Make
      (File_binding.Unexpanded)
      (struct
        let include_keyword = "include"

        let include_allowed_in_versions = `Since (3, 5)

        let non_sexp_behaviour = `User_error
      end)

  let to_file_bindings_expanded ts ~expand_str ~dir =
    Memo.List.concat_map ts ~f:(expand_include ~expand_str ~dir)
    |> Memo.bind
         ~f:
           (Memo.List.map
              ~f:
                (File_binding.Unexpanded.expand ~dir
                   ~f:(expand_str_with_check_for_local_path ~expand_str)))
end
