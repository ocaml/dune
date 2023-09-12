open Stdune

module Re = struct
  include Dune_re

  module Group = struct
    include Group

    let get_opt group n = if Group.test group n then Some (get group n) else None
  end
end

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
  (* CR-someday alizter: more parsed infromation about the patch should go here.
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

module Spec = struct
  type ('path, 'target) t =
    { patch_file : 'path
    ; display : Dune_engine.Display.t
    ; patch_prog : Path.t
    }

  let name = "patch"
  let version = 1

  let bimap { patch_file; display; patch_prog } f _ =
    { patch_file = f patch_file; display; patch_prog }
  ;;

  let is_useful_to ~distribute:_ ~memoize = memoize

  let encode (p : (_, _) t) input _ : Dune_lang.t =
    List [ Dune_lang.atom_or_quoted_string name; input p.patch_file ]
  ;;

  let action (p : (_, _) t) ~ectx:_ ~(eenv : Action.Ext.env) =
    let open Fiber.O in
    let* () = Fiber.return () in
    let input = Value.to_string ~dir:eenv.working_dir (Value.Path p.patch_file) in
    (* Read the patch file. *)
    Io.read_file p.patch_file
    (* Collect all the patches. *)
    |> patches_of_string
    (* Depending on whether it is creating a new file or modifying an existing file
       prepare the files that will be modified accordingly. For modifying existing files
       this means materializing any symlinks or hardlinks. *)
    |> List.iter ~f:(function
      | Patch.New _ -> ()
      | Patch.Delete _ -> ()
      | Patch.Replace file ->
        let file = Path.append_local eenv.working_dir file in
        Io.copy_file ~src:file ~dst:(Path.extend_basename file ~suffix:".for_patch") ();
        Unix.rename (Path.to_string file ^ ".for_patch") (Path.to_string file));
    Process.run
      ~dir:eenv.working_dir
      ~display:p.display
      ~stdout_to:Process.(Io.null Out)
      ~stderr_to:eenv.stderr_to
      ~stdin_from:eenv.stdin_from
      Process.Failure_mode.Strict
      p.patch_prog
      [ "-p1"; "-i"; input ]
  ;;
end

(* CR-someday alizter: This should be an action builder. *)
let action ~display ~patch_prog ~input =
  let module M = struct
    type path = Path.t
    type target = Path.Build.t

    module Spec = Spec

    let v = { Spec.patch_file = input; display; patch_prog }
  end
  in
  Action.Extension (module M)
;;
