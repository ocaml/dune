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
  let line xs = Re.seq ((Re.bol :: xs) @ [ Re.eol ]) in
  let followed_by_line xs = Re.seq [ Re.str "\n"; line xs ] in
  Re.compile
  @@ Re.seq
       [ Re.alt
           [ line [ Re.str {|--- a/|}; Re.group (Re.rep1 Re.notnl) ]
           ; line [ Re.str {|--- |}; Re.group (Re.rep1 Re.notnl) ]
           ]
       ; Re.alt
           [ followed_by_line [ Re.str {|+++ b/|}; Re.group (Re.rep1 Re.notnl) ]
           ; followed_by_line [ Re.str {|+++ |}; Re.group (Re.rep1 Re.notnl) ]
           ]
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
    (* If these are [Some] then they are [/dev/null] *)
    let old_file_dev_null = Re.Group.get_opt group 2 in
    let new_file_dev_null = Re.Group.get_opt group 4 in
    match old_file_dev_null, new_file_dev_null with
    | Some _dev_null, Some _ ->
      (* when both files are /dev/null we don't care about the patch. *)
      None
    | Some _, None ->
      (* New file *)
      let+ new_file = Re.Group.get_opt group 3 in
      Patch.New (Path.Local.of_string new_file)
    | None, Some _ ->
      (* Delete file *)
      let+ new_file = Re.Group.get_opt group 1 in
      Patch.Delete (Path.Local.of_string new_file)
    | None, None ->
      (* Replace file *)
      let+ new_file = Re.Group.get_opt group 3 in
      Patch.Replace (Path.Local.of_string new_file))
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
  let version = 1
  let bimap patch f _ = f patch
  let is_useful_to ~memoize = memoize

  let encode patch input _ : Dune_lang.t =
    List [ Dune_lang.atom_or_quoted_string name; input patch ]
  ;;

  let action patch ~ectx:_ ~(eenv : Action.Ext.env) =
    exec !Dune_engine.Clflags.display ~patch ~dir:eenv.working_dir ~stderr:eenv.stderr_to
  ;;
end

(* CR-someday alizter: This should be an action builder. *)
let action ~patch =
  let module M = struct
    type path = Path.t
    type target = Path.Build.t

    module Spec = Spec

    let v = patch
  end
  in
  Action.Extension (module M)
;;

module For_tests = struct
  let exec = exec
end
