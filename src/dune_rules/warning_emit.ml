open Import
open Memo.O

module Context = struct
  type t =
    | Source_dir_or_enable of Path.Source.t option
    | Project of
        { project : Dune_project.t
        ; vendored : bool
        }

  let project project ~vendored = Project { project; vendored }
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
  (match (context : Context.t) with
   | Project { project; vendored } ->
     Memo.return (if vendored then `Disabled else check_project t project)
   | Source_dir_or_enable None -> Memo.return `Enabled
   | Source_dir_or_enable (Some dir) ->
     let+ dir = Source_tree.nearest_dir dir in
     (match Source_tree.Dir.status dir with
      | Vendored -> `Disabled
      | Normal | Data_only -> check_project t (Source_tree.Dir.project dir)))
  >>= maybe_emit t f
;;

module Bag = struct
  type decode =
    { warning : Warning.t
    ; produce : unit -> User_message.t Memo.t
    }

  type t =
    { context : Context.t
    ; warnings : decode list ref
    }

  let create context = { context; warnings = ref [] }
  let key = Univ_map.Key.create ~name:"warning-bag" Dyn.opaque

  let decode w produce =
    let open Dune_sexp.Decoder in
    Dune_sexp.Decoder.get key
    >>| function
    | None -> Code_error.raise "no warning bag in context" []
    | Some { warnings; _ } -> warnings := { warning = w; produce } :: !warnings
  ;;

  let emit_all { context; warnings } =
    let all = !warnings in
    warnings := [];
    Memo.parallel_iter all ~f:(fun { warning; produce } -> emit warning context produce)
  ;;

  let set t decoder = Dune_sexp.Decoder.set key t decoder
end
