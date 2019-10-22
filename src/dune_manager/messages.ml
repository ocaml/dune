open Stdune
open Result.O
include Messages_intf

let invalid_args args =
  Result.Error
    (Printf.sprintf "invalid arguments:%s"
       (List.fold_left ~init:""
          ~f:(fun a b -> a ^ " " ^ b)
          (List.map ~f:Sexp.to_string args)))

let message_of_sexp =
  let lang_of_sexp = function
    | Sexp.Atom "dune-memory-protocol" :: versions ->
      let decode_version = function
        | Sexp.List [ Sexp.Atom major; Sexp.Atom minor ] ->
          let+ major = Utils.int_of_string ~where:"lang command version" major
          and+ minor =
            Utils.int_of_string ~where:"lang command version" minor
          in
          { major; minor }
        | v ->
          Result.Error
            (Printf.sprintf "invalid version in lang command: %s"
               (Sexp.to_string v))
      in
      Result.List.map ~f:decode_version versions
    | args -> invalid_args args
  and repos_of_sexp args =
    let convert = function
      | Sexp.List
          [ Sexp.List [ Sexp.Atom "dir"; Sexp.Atom directory ]
          ; Sexp.List [ Sexp.Atom "remote"; Sexp.Atom remote ]
          ; Sexp.List [ Sexp.Atom "commit_id"; Sexp.Atom commit ]
          ] ->
        Result.ok { Dune_memory.directory; remote; commit }
      | invalid ->
        Result.Error
          (Printf.sprintf "invalid repo: %s" (Sexp.to_string invalid))
    in
    Result.List.map ~f:convert args
  and promote_of_sexp = function
    | Sexp.List [ Sexp.Atom "key"; Sexp.Atom key ]
      :: Sexp.List (Sexp.Atom "files" :: files)
         :: Sexp.List [ Sexp.Atom "metadata"; Sexp.List metadata ] :: rest as
      cmd ->
      let file = function
        | Sexp.List [ Sexp.Atom path; Sexp.Atom hash ] ->
          Dune_memory.Key.of_string hash
          >>| fun d -> (Path.Build.of_local (Path.Local.of_string path), d)
        | sexp ->
          Result.Error
            (Printf.sprintf "invalid file in promotion message: %s"
               (Sexp.to_string sexp))
      in
      let+ repository =
        match rest with
        | [] -> Result.Ok None
        | [ Sexp.List [ Sexp.Atom "repo"; Sexp.Atom repo ] ] ->
          Result.map ~f:Option.some
            (Utils.int_of_string ~where:"repository index" repo)
        | _ ->
          Result.Error
            (Printf.sprintf "invalid promotion message: %s"
               (Sexp.to_string (Sexp.List cmd)))
      and+ files = Result.List.map ~f:file files
      and+ key = Dune_memory.Key.of_string key in
      { repository; files; key; metadata }
    | args -> invalid_args args
  and path_of_sexp = function
    | [ Sexp.Atom dir ] -> Result.ok (Path.of_string dir)
    | args -> invalid_args args
  in
  function
  | Sexp.List (Sexp.Atom cmd :: args) ->
    Result.map_error
      ~f:(fun s -> cmd ^ ": " ^ s)
      ( match cmd with
      | "lang" ->
        let+ versions = lang_of_sexp args in
        Lang versions
      | "promote" ->
        let+ promotions = promote_of_sexp args in
        Promote promotions
      | "set-build-root" ->
        let+ path = path_of_sexp args in
        SetBuildRoot path
      | "set-common-metadata" -> Result.Ok (SetCommonMetadata args)
      | "set-dune-memory-root" ->
        let+ path = path_of_sexp args in
        SetDuneMemoryRoot path
      | "set-repos" ->
        let+ repos = repos_of_sexp args in
        SetRepos repos
      | _ -> Result.Error (Printf.sprintf "unknown command: %s" cmd) )
  | cmd ->
    Result.Error
      (Printf.sprintf "invalid command format: %s" (Sexp.to_string cmd))
