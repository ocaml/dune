open! Stdune
open! Import
open Memo.Build.O

(* Files and directories have non-overlapping sets of paths, so we can track
   them using the same memoization table. *)
let memo =
  Memo.create "fs_memo" ~doc:"tracking files and directories on the file system"
    ~input:(module Path)
    ~output:(Simple (module Unit))
    ~visibility:Hidden
    (fun _path -> Memo.Build.return ())

(* Declare a dependency on a path. Instead of calling [depend] directly, you
   should prefer using the helper function [declaring_dependency], because it
   calls [depend] and uses the corresponding path in the right order. *)
let depend path =
  if Path.is_in_build_dir path then
    Code_error.raise "Fs_memo.depend called on a build path" [];
  Memo.exec memo path

(* This does two things, in this order:

   - Declare a dependency on [path];

   - Sample the current value using the supplied function [f].

   If the order is reversed, the value can change after we sample it but before
   we register the dependency, which can result in memoizing a stale value. This
   scenario is purely hypothetical (at least for now) but it's nice to rule it
   out explicitly by doing things in the right order.

   Currently, we do not expose this low-level primitive. If you need it, perhaps
   you could add a higher-level primitive instead, such as [file_exists]? *)
let declaring_dependency path ~f =
  let+ () = depend path in
  f path

let file_exists = declaring_dependency ~f:Path.exists

(* CR-someday amokhov: It is unclear if we got the layers of abstraction right
   here. One could argue that caching is a higher-level concept compared to file
   watching, and we should expose this function from the [Cached_digest] module
   instead. For now, we keep it here because it seems nice to group all tracked
   file system access functions in one place, and exposing an uncached version
   of [file_digest] seems error-prone. We may need to rethink this decision. *)
let file_digest = declaring_dependency ~f:Cached_digest.source_or_external_file

let dir_contents = declaring_dependency ~f:Path.readdir_unsorted_with_kinds

module Rebuild_required = struct
  type t =
    | Yes
    | No

  let combine x y =
    match (x, y) with
    | Yes, _ -> Yes
    | _, Yes -> Yes
    | No, No -> No

  let invalidate path =
    match Memo.Expert.previously_evaluated_cell memo path with
    | None -> No
    | Some cell ->
      Memo.Cell.invalidate cell;
      Yes
end

module Event = struct
  type kind =
    | File_created
    | File_deleted
    | File_changed
    | Directory_created
    | Directory_deleted
    | Unknown  (** Treated conservatively as any possible event. *)

  type t =
    { path : Path.t
    ; kind : kind
    }

  let create ~kind ~path =
    if Path.is_in_build_dir path then
      Code_error.raise "Fs_memo.Event.create called on a build path" [];
    { path; kind }

  let handle { kind; path } =
    match kind with
    (* CR-soon amokhov: Improve event handling. *)
    | _ -> (
      let acc = Rebuild_required.invalidate path in
      match Path.parent path with
      | None -> acc
      | Some path ->
        Rebuild_required.combine acc (Rebuild_required.invalidate path))
end

let handle events =
  (* Knowing that the list is non-empty makes it easier to think about the logic
     that checks the [Memo.incremental_mode_enabled] flag. *)
  let events = Nonempty_list.to_list events in
  let rebuild_required =
    (* We can't use [List.exists] here due to its short-circuiting behaviour. *)
    List.fold_left events ~init:No ~f:(fun acc event ->
        Rebuild_required.combine acc (Event.handle event))
  in
  match rebuild_required with
  | Yes -> Rebuild_required.Yes
  | No -> (
    match Memo.incremental_mode_enabled with
    | true -> No
    | false ->
      (* In this mode, we do not assume that all file system dependencies are
         declared correctly and therefore conservatively require a rebuild. *)
      Yes)
