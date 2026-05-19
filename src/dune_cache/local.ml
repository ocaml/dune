open Import

(* See [doc/dev/cache.md] for design and implementation notes. *)

module Store_result = struct
  type t =
    | Stored
    | Already_present
    | Error of exn
    | Will_not_store_due_to_non_determinism of Sexp.t

  let combine x y =
    match x, y with
    | Will_not_store_due_to_non_determinism details, _ ->
      Will_not_store_due_to_non_determinism details
    | _, Will_not_store_due_to_non_determinism details ->
      Will_not_store_due_to_non_determinism details
    | Error e, _ -> Error e
    | _, Error e -> Error e
    | Stored, _ -> Stored
    | _, Stored -> Stored
    | Already_present, Already_present -> Already_present
  ;;

  let empty = Already_present
end

module Restore_result = struct
  type 'data t =
    | Restored of 'data
    | Not_found_in_cache
    | Error of exn

  let bind t ~f =
    match t with
    | Restored data -> f data
    | (Not_found_in_cache | Error _) as res -> res
  ;;

  let map t ~f =
    match t with
    | Restored data -> Restored (f data)
    | (Not_found_in_cache | Error _) as res -> res
  ;;
end

let restore_file_content path : string Restore_result.t =
  match Io.read_file ~binary:false path with
  | contents -> Restored contents
  | exception Sys_error (_some_error_message : string) ->
    (* CR-someday amokhov: [Io.read_file] doesn't raise "typed" exceptions like
       [Unix_error], so we guess here that the exception means "file not found".
       Can we make the API of [Io] more precise? *)
    Not_found_in_cache
  | exception e ->
    (* This code path might be unreachable until the above is resolved. *)
    Error e
;;

let restore_metadata_file file ~of_sexp : _ Restore_result.t =
  restore_file_content file
  |> Restore_result.bind ~f:(fun content ->
    match Csexp.parse_string content with
    | Error (_offset, msg) -> Error (Failure msg)
    | Ok sexp ->
      (match of_sexp sexp with
       | Ok content -> Restored content
       | Error e -> Error e))
;;

module Artifacts = struct
  module Metadata_entry = struct
    type t =
      { path : Path.Local.t
      ; digest : Digest.t option
      }

    let equal x y =
      Path.Local.equal x.path y.path && Option.equal Digest.equal x.digest y.digest
    ;;

    let digest_to_sexp = function
      | None -> Sexp.Atom "<dir>"
      | Some digest -> Sexp.Atom (Digest.to_string digest)
    ;;

    let to_sexp { path; digest } =
      Sexp.List [ Atom (Path.Local.to_string path); digest_to_sexp digest ]
    ;;

    let digest_of_sexp = function
      | "<dir>" -> Ok None
      | digest ->
        (match Digest.from_hex digest with
         | Some digest -> Ok (Some digest)
         | None ->
           Error
             (Failure
                (sprintf "Cannot parse file digest %S in cache metadata entry" digest)))
    ;;

    let of_sexp = function
      | Sexp.List [ Atom path; Atom digest ] ->
        (match digest_of_sexp digest with
         | Ok digest -> Ok { path = Path.Local.of_string path; digest }
         | Error e -> Error e)
      | _ -> Error (Failure "Cannot parse cache metadata entry")
    ;;
  end

  module Metadata_file = struct
    type t =
      { (* The entries are listed in the same order that they were provided when
           storing artifacts in the cache. We keep the order to avoid confusion
           even though sorting the entres is tempting. *)
        entries : Metadata_entry.t list
      }

    let to_sexp { entries } =
      Sexp.List
        [ List [ Atom "metadata" ]
        ; List (Atom "files" :: List.map entries ~f:Metadata_entry.to_sexp)
        ]
    ;;

    let of_sexp = function
      | Sexp.List [ List (Atom "metadata" :: _metadata); List (Atom "files" :: entries) ]
        ->
        let entries = List.map entries ~f:Metadata_entry.of_sexp in
        (match Result.List.all entries with
         | Ok entries -> Ok { entries }
         | Error e -> Error e)
      | _ -> Error (Failure "Cannot parse cache metadata")
    ;;

    module Matches_existing_query = struct
      type t =
        | Match
        | Mismatch of Sexp.t
    end

    let matches_existing_entry t ~existing_content : Matches_existing_query.t =
      match Csexp.parse_string existing_content with
      | Error _ -> Mismatch (Atom "Malformed value in cache")
      | Ok sexp ->
        (match of_sexp sexp with
         | Error _ -> Mismatch (Atom "Malformed value in cache")
         | Ok existing ->
           (match List.equal Metadata_entry.equal t.entries existing.entries with
            | true -> Match
            | false ->
              Mismatch
                (Sexp.record
                   [ ( "in_cache"
                     , Sexp.List (List.map ~f:Metadata_entry.to_sexp existing.entries) )
                   ; "computed", Sexp.List (List.map ~f:Metadata_entry.to_sexp t.entries)
                   ])))
    ;;

    let store entries ~mode ~rule_digest : Store_result.t =
      let metadata = { entries } in
      let path_in_cache =
        Lazy.force (Layout.metadata_path ~rule_or_action_digest:rule_digest)
      in
      match
        let content = Csexp.to_string (to_sexp metadata) in
        Util.write_atomically ~mode ~content path_in_cache
      with
      | Ok -> Stored
      | Error e -> Error e
      | Already_present ->
        (match restore_file_content path_in_cache with
         | Not_found_in_cache ->
           (* This can actually happen, but we think it's an unlikely case. The
              [Already_present] branch should already be rarely visited (only if
              multiple build systems attempt to store the same entry), but also a
              trimmer must be running in parallel to delete this file. *)
           Error (Failure "Race in artifact metadata store")
         | Error e -> Error e
         | Restored existing_content ->
           (match matches_existing_entry metadata ~existing_content with
            | Mismatch details -> Will_not_store_due_to_non_determinism details
            | Match ->
              (* At this point we could in principle overwrite the existing metadata
                 file with the new [content] because it seems fresher. We choose not
                 to do that because in practice we end up here only due to racing. The
                 racing processes are not totally ordered, so neither content is
                 really fresher than the other. *)
              Already_present))
    ;;

    let restore ~rule_digest =
      Layout.metadata_path ~rule_or_action_digest:rule_digest
      |> Lazy.force
      |> restore_metadata_file ~of_sexp
    ;;
  end

  let list ~rule_digest =
    Metadata_file.restore ~rule_digest
    |> Restore_result.map ~f:(fun ({ entries } : Metadata_file.t) -> entries)
  ;;
end

module Metadata = struct
  let restore ~metadata_path ~rule_or_action_digest =
    metadata_path ~rule_or_action_digest
    |> Lazy.force
    |> restore_metadata_file ~of_sexp:Artifacts.Metadata_file.of_sexp
  ;;

  module Versioned = struct
    let restore version = restore ~metadata_path:(Layout.Versioned.metadata_path version)
  end
end
