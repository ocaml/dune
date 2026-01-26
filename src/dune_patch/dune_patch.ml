open Stdune

include struct
  open Dune_engine
  module Action = Action
  module Process = Process
  module Utils = Utils
  module Action_builder = Action_builder
end

include struct
  open Dune_lang
  module Value = Value
end

let re =
  let line xs = Re.seq ((Re.bol :: xs) @ [ Re.eol ]) in
  let followed_by_line xs = Re.seq [ Re.str "\n"; line xs ] in
  let filename = Re.group (Re.rep1 (Re.compl [ Re.space ])) in
  (* We don't care about what's after the filename. (likely a timestamp) *)
  let junk = Re.rep Re.notnl in
  Re.compile
  @@ Re.seq
       [ line [ Re.str {|--- |}; filename; junk ]
       ; followed_by_line [ Re.str {|+++ |}; filename; junk ]
       ]
;;

module Patch = struct
  (* CR-someday alizter: more parsed information about the patch should go here.
     Eventually we wish to replace the patch command inside the patch action with a pure
     OCaml implementation. *)
  type operation =
    | New of Path.Local.t
    | Delete of Path.Local.t
    | Replace of Path.Local.t

  type t =
    { prefix : int
    ; op : operation
    }
end

let patches_of_string patch_string =
  Re.all re patch_string
  |> List.filter_map ~f:(fun group ->
    let open Option.O in
    (* A match failure means a file name couldn't be parsed. *)
    let* old_file = Re.Group.get_opt group 1 in
    let* new_file = Re.Group.get_opt group 2 in
    match old_file = "/dev/null", new_file = "/dev/null" with
    | true, true ->
      (* when both files are /dev/null we don't care about the patch. *)
      None
    | true, false ->
      (* New file *)
      let path = Path.Local.of_string new_file in
      let prefix, path =
        match Path.Local.split_first_component path with
        | Some ("b", path) -> 1, path
        | _ -> 0, path
      in
      Some { Patch.op = Patch.New path; prefix }
    | false, true ->
      (* Delete file *)
      let path = Path.Local.of_string old_file in
      let prefix, path =
        match Path.Local.split_first_component path with
        | Some ("a", path) -> 1, path
        | _ -> 0, path
      in
      Some { Patch.op = Patch.Delete path; prefix }
    | false, false ->
      let old_path = Path.Local.of_string old_file in
      let new_path = Path.Local.of_string new_file in
      let new_path, prefix =
        match
          ( Path.Local.split_first_component old_path
          , Path.Local.split_first_component new_path )
        with
        | Some (_, old_path), Some (_, new_path)
          when Path.Local.equal old_path new_path && not (Path.Local.is_root new_path) ->
          (* suffixes are the same and not empty *)
          new_path, 1
        | _, _ -> new_path, 0
      in
      (* Replace file *)
      Some { Patch.op = Patch.Replace new_path; prefix })
;;

let prog =
  lazy
    (let path = Env_path.path Env.initial in
     let bins = [ "gpatch"; "patch" ] in
     match List.find_map bins ~f:(Bin.which ~path) with
     | Some p -> p
     | None -> User_error.raise [ Pp.textf "%s not found." (String.enumerate_and bins) ])
;;

let exec display ~patch ~dir ~stderr =
  let open Fiber.O in
  let* () = Fiber.return () in
  let patches =
    patch
    (* Read the patch file. *)
    |> Io.read_file
    (* Collect all the patches. *)
    |> patches_of_string
  in
  List.iter patches ~f:(fun { Patch.op; prefix = _ } ->
    (* Depending on whether it is creating a new file or modifying an existing file
       prepare the files that will be modified accordingly. For modifying existing files
       this means materializing any symlinks or hardlinks. *)
    match op with
    | Patch.New _ | Delete _ -> ()
    | Replace file ->
      let file = Path.append_local dir file in
      let temp = Path.extend_basename file ~suffix:".for_patch" in
      Io.copy_file ~src:file ~dst:temp ();
      Unix.rename (Path.to_string temp) (Path.to_string file));
  match patches with
  | [] -> User_error.raise [ Pp.text "No patches in patch file detected" ]
  | { Patch.op = _; prefix } :: patches ->
    (match
       List.for_all ~f:(fun { Patch.op = _; prefix = p } -> Int.equal prefix p) patches
     with
     | false ->
       User_error.raise [ Pp.text "Different prefix lengths in file unsupported" ]
     | true ->
       let p_flag = sprintf "-p%d" prefix in
       Process.run
         ~dir
         ~display
         ~stdout_to:Process.(Io.null Out)
         ~stderr_to:stderr
         ~stdin_from:Process.(Io.null In)
         Process.Failure_mode.Strict
         (Lazy.force prog)
         [ p_flag; "-i"; Path.reach_for_running ~from:dir patch ])
;;

module Spec = struct
  type ('path, 'target) t = 'path

  let name = "patch"
  let version = 2
  let bimap patch f _ = f patch
  let is_useful_to ~memoize = memoize
  let encode patch input _ : Sexp.t = input patch

  let action patch ~ectx:_ ~(eenv : Action.env) =
    exec !Dune_engine.Clflags.display ~patch ~dir:eenv.working_dir ~stderr:eenv.stderr_to
  ;;
end

(* CR-someday alizter: This should be an action builder. *)
module Action = Action_ext.Make (Spec)

let action ~patch = Action.action patch

module For_tests = struct
  let exec = exec
end
