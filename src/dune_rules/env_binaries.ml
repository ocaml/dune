open Import
open Memo.O

let expand_str_lazy expander sw =
  match String_with_vars.text_only sw with
  | Some s -> Memo.return s
  | None ->
    let open Memo.O in
    let* expander = Memo.Lazy.force expander in
    Expander.No_deps.expand_str expander sw
;;

let impl dir =
  match Install.Context.of_path dir with
  | None ->
    Code_error.raise "no context for this directory" [ "dir", Path.Build.to_dyn dir ]
  | Some ctx ->
    let* binaries =
      Dune_load.stanzas_in_dir dir
      >>= function
      | None -> Memo.return []
      | Some stanzas ->
        let* profile = Per_context.profile ctx in
        Dune_file.find_stanzas stanzas Dune_env.key
        >>| (function
               | [ config ] -> Some config
               | [] -> None
               | _ :: _ :: _ -> assert false)
        >>| (function
         | None -> []
         | Some stanza ->
           (match Dune_env.find_opt stanza ~profile with
            | None -> []
            | Some env -> Option.value env.binaries ~default:[]))
    in
    let expander =
      Memo.lazy_ (fun () -> Super_context.find_exn ctx >>= Super_context.expander ~dir)
    in
    Memo.parallel_map
      binaries
      ~f:(File_binding.Unexpanded.expand ~dir ~f:(expand_str_lazy expander))
;;

let get =
  let memo = Memo.create "env-binaries" ~input:(module Path.Build) impl in
  fun ~dir -> Memo.exec memo dir
;;
