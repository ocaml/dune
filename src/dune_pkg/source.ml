open Import

type t =
  { url : Loc.t * OpamUrl.t
  ; checksum : (Loc.t * Checksum.t) option
  }

let remove_locs { url = _loc, url; checksum } =
  { url = Loc.none, url
  ; checksum = Option.map checksum ~f:(fun (_loc, checksum) -> Loc.none, checksum)
  }
;;

let equal
  { url = loc, url; checksum }
  { url = other_loc, other_url; checksum = other_checksum }
  =
  Loc.equal loc other_loc
  && OpamUrl.equal url other_url
  && Option.equal
       (fun (loc, checksum) (other_loc, other_checksum) ->
         Loc.equal loc other_loc && Checksum.equal checksum other_checksum)
       checksum
       other_checksum
;;

let to_dyn { url = _loc, url; checksum } =
  Dyn.record
    [ "url", Dyn.string (OpamUrl.to_string url)
    ; "checksum", Dyn.option (fun (_loc, checksum) -> Checksum.to_dyn checksum) checksum
    ]
;;

let fetch_and_hash_archive_cached =
  let cache = Single_run_file_cache.create () in
  fun (url_loc, url) ->
    let open Fiber.O in
    Single_run_file_cache.with_ cache ~key:(OpamUrl.to_string url) ~f:(fun target ->
      Fetch.fetch_without_checksum ~unpack:false ~target ~url:(url_loc, url))
    >>| function
    | Ok target -> Some (Dune_digest.file target |> Checksum.of_dune_digest)
    | Error message_opt ->
      let message =
        match message_opt with
        | Some message -> message
        | None ->
          User_message.make
            [ Pp.textf
                "Failed to retrieve source archive from: %s"
                (OpamUrl.to_string url)
            ]
      in
      User_warning.emit_message message;
      None
;;

let compute_missing_checksum
  ({ url = url_loc, url; checksum } as fetch)
  package_name
  ~pinned
  =
  let open Fiber.O in
  match checksum with
  | Some _ -> Fiber.return fetch
  | None when OpamUrl.is_local url || OpamUrl.is_version_control url -> Fiber.return fetch
  | None ->
    if not pinned
       (* No point in warning this about pinned packages. The user explicitly
          asked for the pins *)
    then
      User_message.print
        (User_message.make
           [ Pp.textf
               "Package %S has source archive which lacks a checksum."
               (Package_name.to_string package_name)
           ; Pp.textf
               "The source archive will be downloaded from: %s"
               (OpamUrl.to_string url)
           ; Pp.text "Dune will compute its own checksum for this source archive."
           ]);
    fetch_and_hash_archive_cached (url_loc, url)
    >>| Option.map ~f:(fun checksum ->
      { url = url_loc, url; checksum = Some (Loc.none, checksum) })
    >>| Option.value ~default:fetch
;;

module Fields = struct
  let copy = "copy"
  let fetch = "fetch"
  let url = "url"
  let checksum = "checksum"
end

let decode_fetch =
  let open Decoder in
  let+ url_loc, url = field Fields.url OpamUrl.decode_loc
  and+ checksum = field_o Fields.checksum (located string) in
  let checksum =
    match checksum with
    | None -> None
    | Some ((loc, _) as checksum) ->
      let checksum = Checksum.of_string_user_error checksum |> User_error.ok_exn in
      Some (loc, checksum)
  in
  { url = url_loc, url; checksum }
;;

let external_copy (loc, path) =
  let path = Path.External.to_string path in
  let url : OpamUrl.t = { transport = "file"; path; hash = None; backend = `rsync } in
  { url = loc, url; checksum = None }
;;

let decode =
  let open Decoder in
  sum
    [ ( Fields.copy
      , located string
        >>| fun (loc, source) path ->
        let path =
          if Filename.is_relative source
          then Path.External.relative path source
          else Path.External.of_string source
        in
        external_copy (loc, path) )
    ; ( Fields.fetch
      , let+ fetch = fields decode_fetch in
        fun _ -> fetch )
    ]
;;

let encode_fetch_field { url = _loc, url; checksum } =
  let open Encoder in
  [ field Fields.url string (OpamUrl.to_string url)
  ; field_o Fields.checksum Checksum.encode (Option.map checksum ~f:snd)
  ]
;;

let encode t =
  let open Encoder in
  named_record_fields Fields.fetch (encode_fetch_field t)
;;

let kind t =
  let _, url = t.url in
  if OpamUrl0.is_local url && url.backend = `rsync
  then `Directory_or_archive (Path.External.of_string url.path)
  else `Fetch
;;
