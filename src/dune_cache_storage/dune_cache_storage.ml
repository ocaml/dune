open Stdune
open Import
module Layout = Layout
module Mode = Mode
module Util = Util
module Version = Version

(* See [doc/dev/cache.md] for design and implementation notes. *)

module Store_result = struct
  type t =
    | Stored
    | Already_present
    | Error of exn
    | Will_not_store_due_to_non_determinism of Sexp.t

  let combine x y =
    match (x, y) with
    | Will_not_store_due_to_non_determinism details, _ ->
      Will_not_store_due_to_non_determinism details
    | _, Will_not_store_due_to_non_determinism details ->
      Will_not_store_due_to_non_determinism details
    | Error e, _ -> Error e
    | _, Error e -> Error e
    | Stored, _ -> Stored
    | _, Stored -> Stored
    | Already_present, Already_present -> Already_present

  let empty = Already_present

  let of_write_result (t : Util.Write_result.t) =
    match t with
    | Ok -> Stored
    | Already_present -> Already_present
    | Error exn -> Error exn
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

  let map t ~f =
    match t with
    | Restored data -> Restored (f data)
    | (Not_found_in_cache | Error _) as res -> res
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

module Matches_existing_query = struct
  type t =
    | Match
    | Mismatch of Sexp.t
end

(* Store [metadata] corresponding to a given [rule_or_action_digest] to the
   cache using the supplied [to_sexp] serializer. If the cache already contains
   an entry for the hash, we use [matches_existing_entry] to check that the
   given [content] matches the previously stored one. If this is not the case,
   we return [Will_not_store_due_to_non_determinism]. *)
let store_metadata ~mode ~rule_or_action_digest ~metadata ~to_sexp
    ~matches_existing_entry : Store_result.t =
  let content = Csexp.to_string (to_sexp metadata) in
  let path_in_cache = Layout.metadata_path ~rule_or_action_digest in
  match Util.write_atomically ~mode ~content path_in_cache with
  | Ok -> Stored
  | Error e -> Error e
  | Already_present -> (
    match restore_file_content path_in_cache with
    | Not_found_in_cache ->
      (* This can actually happen, but we think it's an unlikely case. The
         [Already_present] branch should already be rarely visited (only if
         multiple build systems attempt to store the same entry), but also a
         trimmer must be running in parallel to delete this file. *)
      Error (Failure "Race in store_metadata")
    | Error e -> Error e
    | Restored existing_content -> (
      match
        (matches_existing_entry metadata ~existing_content
          : Matches_existing_query.t)
      with
      | Mismatch details -> Will_not_store_due_to_non_determinism details
      | Match ->
        (* At this point we could in principle overwrite the existing metadata
           file with the new [content] because it seems fresher. We choose not
           to do that because in practice we end up here only due to racing. The
           racing processes are not totally ordered, so neither content is
           really fresher than the other. *)
        Already_present))

let restore_metadata_file file ~of_sexp : _ Restore_result.t =
  Restore_result.bind (restore_file_content file) ~f:(fun content ->
      match Csexp.parse_string content with
      | Ok sexp -> (
        match of_sexp sexp with
        | Ok content -> Restored content
        | Error e -> Error e)
      | Error (_offset, msg) -> Error (Failure msg))

(* Read a metadata file corresponding to a given [rule_or_action_digest] from
   the cache and parse it using the supplied [of_sexp] parser. *)
let restore_metadata ~rule_or_action_digest ~of_sexp : _ Restore_result.t =
  restore_metadata_file (Layout.metadata_path ~rule_or_action_digest) ~of_sexp

module Raw_value = struct
  let store_unchecked ~mode ~content ~content_digest =
    Util.write_atomically ~mode ~content
      (Layout.value_path ~value_digest:content_digest)
end

module Value = struct
  module Metadata_file = struct
    type t =
      { metadata : Sexp.t list
      ; value_digest : Digest.t
      }

    let to_sexp { metadata; value_digest } =
      Sexp.List
        [ List (Atom "metadata" :: metadata)
        ; List [ Atom "value"; Sexp.Atom (Digest.to_string value_digest) ]
        ]

    let of_sexp = function
      | Sexp.List
          [ List (Atom "metadata" :: metadata)
          ; List [ Atom "value"; Sexp.Atom value_hash ]
          ] -> (
        match Digest.from_hex value_hash with
        | Some value_digest -> Ok { metadata; value_digest }
        | None ->
          Error (Failure "Cannot parse cache metadata: malformed value digest"))
      | _ -> Error (Failure "Cannot parse cache metadata")

    let restore ~action_digest =
      restore_metadata ~rule_or_action_digest:action_digest ~of_sexp

    let matches_existing_entry t ~existing_content : Matches_existing_query.t =
      match Csexp.parse_string existing_content with
      | Error _ -> Mismatch (Atom "Malformed value in cache")
      | Ok sexp -> (
        match of_sexp sexp with
        | Error _ -> Mismatch (Atom "Malformed value in cache")
        | Ok existing -> (
          match Digest.equal t.value_digest existing.value_digest with
          | true -> Match
          | false ->
            Mismatch
              (Sexp.record
                 [ ("in_cache", Atom (Digest.to_string existing.value_digest))
                 ; ("computed", Atom (Digest.to_string t.value_digest))
                 ])))
  end

  let store ~mode ~action_digest value : Store_result.t =
    let value_digest = Digest.string value in
    let metadata : Metadata_file.t = { metadata = []; value_digest } in
    match
      store_metadata ~mode ~rule_or_action_digest:action_digest ~metadata
        ~to_sexp:Metadata_file.to_sexp
        ~matches_existing_entry:Metadata_file.matches_existing_entry
    with
    | Will_not_store_due_to_non_determinism details ->
      Will_not_store_due_to_non_determinism details
    | Error e -> Error e
    | (Already_present | Stored) as metadata_result ->
      (* We assume that there are no hash collisions and hence omit the check
         for non-determinism when writing values. *)
      let value_result =
        Raw_value.store_unchecked ~mode ~content:value
          ~content_digest:value_digest
        |> Store_result.of_write_result
      in
      Store_result.combine metadata_result value_result

  let restore ~action_digest =
    Restore_result.bind (Metadata_file.restore ~action_digest)
      ~f:(fun ({ value_digest; _ } : Metadata_file.t) ->
        restore_file_content (Layout.value_path ~value_digest))
end

module Artifacts = struct
  module Metadata_entry = struct
    type t =
      { file_name : string
      ; file_digest : Digest.t
      }

    let equal x y =
      Digest.equal x.file_digest y.file_digest
      && String.equal x.file_name y.file_name

    let to_sexp { file_name; file_digest } =
      Sexp.List [ Atom file_name; Atom (Digest.to_string file_digest) ]

    let of_sexp = function
      | Sexp.List [ Atom file_name; Atom file_digest ] -> (
        match Digest.from_hex file_digest with
        | Some file_digest -> Ok { file_name; file_digest }
        | None ->
          Error
            (Failure
               (sprintf "Cannot parse file digest %s in cache metadata entry"
                  file_digest)))
      | _ -> Error (Failure "Cannot parse cache metadata entry")
  end

  module Metadata_file = struct
    type t =
      { metadata : Sexp.t list
      ; (* The entries are listed in the same order that they were provided when
           storing artifacts in the cache. We keep the order to avoid confusion
           even though sorting the entres is tempting. *)
        entries : Metadata_entry.t list
      }

    let to_sexp { metadata; entries } =
      Sexp.List
        [ List (Atom "metadata" :: metadata)
        ; List (Atom "files" :: List.map entries ~f:Metadata_entry.to_sexp)
        ]

    let of_sexp = function
      | Sexp.List
          [ List (Atom "metadata" :: metadata); List (Atom "files" :: entries) ]
        -> (
        let entries = List.map entries ~f:Metadata_entry.of_sexp in
        match Result.List.all entries with
        | Ok entries -> Ok { metadata; entries }
        | Error e -> Error e)
      | _ -> Error (Failure "Cannot parse cache metadata")

    let matches_existing_entry t ~existing_content : Matches_existing_query.t =
      match Csexp.parse_string existing_content with
      | Error _ -> Mismatch (Atom "Malformed value in cache")
      | Ok sexp -> (
        match of_sexp sexp with
        | Error _ -> Mismatch (Atom "Malformed value in cache")
        | Ok existing -> (
          match List.equal Metadata_entry.equal t.entries existing.entries with
          | true -> Match
          | false ->
            Mismatch
              (Sexp.record
                 [ ( "in_cache"
                   , Sexp.List
                       (List.map ~f:Metadata_entry.to_sexp existing.entries) )
                 ; ( "computed"
                   , Sexp.List (List.map ~f:Metadata_entry.to_sexp t.entries) )
                 ])))

    let store t ~mode ~rule_digest =
      store_metadata ~mode ~rule_or_action_digest:rule_digest ~metadata:t
        ~to_sexp ~matches_existing_entry

    let restore ~rule_digest =
      restore_metadata ~rule_or_action_digest:rule_digest ~of_sexp
  end

  let list ~rule_digest =
    Restore_result.map (Metadata_file.restore ~rule_digest)
      ~f:(fun ({ entries; _ } : Metadata_file.t) -> entries)
end

module Metadata = struct
  type t =
    | Artifacts of Artifacts.Metadata_file.t
    | Value of Value.Metadata_file.t

  let of_sexp sexp : (t, exn) result =
    match Artifacts.Metadata_file.of_sexp sexp with
    | Ok res -> Ok (Artifacts res)
    | Error _exn ->
      (* CR-someday amokhov: Here we are discarding the [_exn] but it may be
         better to combine the two exceptions when both parsers fail. *)
      Result.map (Value.Metadata_file.of_sexp sexp) ~f:(fun res -> Value res)

  let restore ~metadata_path ~rule_or_action_digest =
    restore_metadata_file (metadata_path ~rule_or_action_digest) ~of_sexp

  module Versioned = struct
    let restore version =
      restore ~metadata_path:(Layout.Versioned.metadata_path version)
  end

  let restore = restore ~metadata_path:Layout.metadata_path
end

let with_temp_file ?(prefix = "dune") ~suffix f =
  Fiber_util.Temp.with_temp_file ~dir:Layout.temp_dir ~prefix ~suffix ~f

let with_temp_dir ?(prefix = "dune") ~suffix f =
  Fiber_util.Temp.with_temp_dir ~parent_dir:Layout.temp_dir ~prefix ~suffix ~f
