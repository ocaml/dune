open Stdune
module Re = Dune_re

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
  let open Re in
  let line xs = seq ((bol :: xs) @ [ eol ]) in
  let filename = group @@ rep1 @@ compl [ space ] in
  (* We don't care about what's after the filename. (likely a timestamp) *)
  let junk = rep notnl in
  compile
  @@ seq
       [ line [ str {|--- |}; opt (str "a/"); filename; junk ]
       ; str "\n"
       ; line [ str {|+++ |}; opt (str "b/"); filename; junk ]
       ]
;;

module Patch = struct
  (* CR-someday alizter: more parsed information about the patch should go here.
     Eventually we wish to replace the patch command inside the patch action with a pure
     OCaml implementation. *)
  type t =
    | New of Path.Local.t
    | Delete of Path.Local.t
    | Replace of Path.Local.t
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
      Some (Patch.New (Path.Local.of_string new_file))
    | false, true ->
      (* Delete file *)
      Some (Patch.Delete (Path.Local.of_string old_file))
    | false, false ->
      (* Replace file *)
      Some (Patch.Replace (Path.Local.of_string new_file)))
;;

let prog =
  lazy
    (match Bin.which ~path:(Env_path.path Env.initial) "patch" with
     | Some p -> p
     | None -> User_error.raise [ Pp.text "patch not found" ])
;;

let exec display ~patch ~dir ~stderr =
  let open Fiber.O in
  let* () = Fiber.return () in
  (* Read the patch file. *)
  Io.read_file patch
  (* Collect all the patches. *)
  |> patches_of_string
  (* Depending on whether it is creating a new file or modifying an existing file
     prepare the files that will be modified accordingly. For modifying existing files
     this means materializing any symlinks or hardlinks. *)
  |> List.iter ~f:(function
    | Patch.New _ | Delete _ -> ()
    | Replace file ->
      let file = Path.append_local dir file in
      let temp = Path.extend_basename file ~suffix:".for_patch" in
      Io.copy_file ~src:file ~dst:temp ();
      Path.rename temp file);
  Process.run
    ~dir
    ~display
    ~stdout_to:Process.(Io.null Out)
    ~stderr_to:stderr
    ~stdin_from:Process.(Io.null In)
    Process.Failure_mode.Strict
    (Lazy.force prog)
    [ "-p1"; "-i"; Path.reach_for_running ~from:dir patch ]
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
