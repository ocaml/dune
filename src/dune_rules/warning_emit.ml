open Import
open Memo.O

module Context = struct
  type t =
    | Source_dir_or_enable of Path.Source.t option
    | Project of Dune_project.t

  let project p = Project p
  let source_dir_or_enable p = Source_dir_or_enable p
end

let check_project warning project =
  let warnings = Dune_project.warnings project in
  let version = Dune_project.dune_version project in
  Warning.Settings.active warnings warning version
;;

let emit_hint warning =
  [ Pp.concat
      ~sep:Pp.space
      [ Pp.text "To disable this warning, add the following to your dune-project file:"
      ; Pp.verbatim (sprintf "(warnings (%s disabled))" (Warning.name warning))
      ]
  ]
;;

let maybe_emit warning f = function
  | `Disabled -> Memo.return ()
  | `Enabled ->
    let+ message =
      let+ message = f () in
      let hints = emit_hint warning in
      { message with User_message.hints }
    in
    User_warning.emit_message message
;;

let emit t context f =
  (let+ dir =
     match
       match (context : Context.t) with
       | Source_dir_or_enable dir -> dir
       | Project project -> Some (Dune_project.root project)
     with
     | None -> Memo.return None
     | Some dir -> Source_tree.nearest_dir dir >>| Option.some
   in
   match dir with
   | None -> `Enabled
   | Some dir ->
     (match Source_tree.Dir.status dir with
      | Vendored -> `Disabled
      | _ -> check_project t (Source_tree.Dir.project dir)))
  >>= maybe_emit t f
;;

let emit_project warning project message =
  match check_project warning project with
  | `Disabled -> ()
  | `Enabled ->
    let message =
      let hints = emit_hint warning in
      { message with User_message.hints }
    in
    User_warning.emit_message message
;;

module Bag = struct
  type decode =
    { active : Config.Toggle.t
    ; warning : Warning.t
    ; project_root : Path.Source.t option
    ; produce : unit -> User_message.t Memo.t
    }

  let decode warning produce =
    let open Dune_sexp.Decoder in
    Dune_project.get ()
    >>| function
    | None -> { warning; active = `Enabled; project_root = None; produce }
    | Some project ->
      { active = check_project warning project
      ; project_root = Some (Dune_project.root project)
      ; produce
      ; warning
      }
  ;;

  let emit_decode { warning; active; project_root; produce } =
    (match project_root with
     | None -> Memo.return active
     | Some project_root ->
       Source_tree.nearest_dir project_root
       >>| Source_tree.Dir.status
       >>| (function
        | Vendored -> `Disabled
        | _ -> active))
    >>= maybe_emit warning produce
  ;;

  type t = decode list ref

  let create () = ref []
  let key = Univ_map.Key.create ~name:"warning-bag" Dyn.opaque

  let decode w produce =
    let open Dune_sexp.Decoder in
    let* decode = decode w produce in
    Dune_sexp.Decoder.get key
    >>| function
    | None -> Code_error.raise "no warning bag in context" []
    | Some p -> p := decode :: !p
  ;;

  let emit_all t =
    let all = !t in
    t := [];
    Memo.parallel_iter all ~f:emit_decode
  ;;

  let set t decoder = Dune_sexp.Decoder.set key t decoder
end
