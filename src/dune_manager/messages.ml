open Stdune
open Result.O
include Messages_intf

let invalid_args args =
  Result.Error
    (Printf.sprintf "invalid arguments:%s"
       (String.concat ~sep:" " (List.map ~f:Sexp.to_string args)))

let sexp_of_message : type a. a message -> Sexp.t =
  let cmd name args = Sexp.List (Sexp.Atom name :: args) in
  function
  | Lang versions ->
    cmd "lang"
      ( Sexp.Atom "dune-memory-protocol"
      :: (List.map ~f:(fun { major; minor } ->
              Sexp.List
                [ Sexp.Atom (string_of_int major)
                ; Sexp.Atom (string_of_int minor)
                ]))
           versions )
  | Promote promotion ->
    let key = Dune_memory.Key.to_string promotion.key
    and f (path, digest) =
      Sexp.List
        [ Sexp.Atom (Path.Local.to_string (Path.Build.local path))
        ; Sexp.Atom (Digest.to_string digest)
        ]
    and repo =
      match promotion.repository with
      | Some idx ->
        [ Sexp.List [ Sexp.Atom "repo"; Sexp.Atom (string_of_int idx) ] ]
      | None -> []
    in
    cmd "promote"
      ( Sexp.List [ Sexp.Atom "key"; Sexp.Atom key ]
      :: Sexp.List (Sexp.Atom "files" :: List.map ~f promotion.files)
      :: Sexp.List [ Sexp.Atom "metadata"; Sexp.List promotion.metadata ]
      :: repo )
  | SetBuildRoot root ->
    cmd "set-build-root" [ Sexp.Atom (Path.to_absolute_filename root) ]
  | SetCommonMetadata metadata -> cmd "set-common-metadata" metadata
  | SetRepos repositories ->
    let f { Dune_memory.directory; remote; commit } =
      Sexp.List
        [ Sexp.List [ Sexp.Atom "dir"; Sexp.Atom directory ]
        ; Sexp.List [ Sexp.Atom "remote"; Sexp.Atom remote ]
        ; Sexp.List [ Sexp.Atom "commit_id"; Sexp.Atom commit ]
        ]
    in
    cmd "set-repos" (List.map ~f repositories)
  | Dedup f ->
    cmd "dedup"
      [ Sexp.List
          [ Sexp.Atom
              (Path.Local.to_string (Path.Build.local f.in_the_build_directory))
          ; Sexp.Atom (Path.to_string f.in_the_memory)
          ; Sexp.Atom (Digest.to_string f.digest)
          ]
      ]

let incoming_message_of_sexp = function
  | Sexp.List
      [ Sexp.Atom "dedup"
      ; Sexp.List [ Sexp.Atom source; Sexp.Atom target; Sexp.Atom digest ]
      ] -> (
    match Digest.from_hex digest with
    | Some digest ->
      Result.Ok
        (Dedup
           { in_the_build_directory = Path.Build.of_string source
           ; in_the_memory = Path.of_string target
           ; digest
           })
    | None -> Result.Error (Printf.sprintf "invalid digest: %s" digest) )
  | exp ->
    Result.Error (Printf.sprintf "invalid command: %s" (Sexp.to_string exp))

let outgoing_message_of_sexp =
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
      | "set-repos" ->
        let+ repos = repos_of_sexp args in
        SetRepos repos
      | _ -> Result.Error (Printf.sprintf "unknown command: %s" cmd) )
  | cmd ->
    Result.Error
      (Printf.sprintf "invalid command format: %s" (Sexp.to_string cmd))
