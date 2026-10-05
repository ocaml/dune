open Import

module DB = struct
  (* Needed to tell resolve the configuration of sources merlin gives us.

     This is all ugly and doesn't work well for watch mode, but it's better
     than the old hack. It's temporary until we have something RPC based.
  *)
  module Persistent = Dune_util.Persistent.Make (struct
      type nonrec t = Path.Build.t Path.Build.Table.t

      let name = "COPY-LINE-DIRECTIVE-MAP"
      let sharing = true
      let version = 4
      let repr = Repr.abstract (Path.Build.Table.to_dyn Path.Build.to_dyn)
    end)

  let needs_dumping = ref false
  let file = Path.relative Path.build_dir ".copy-db"

  let t =
    (* This mutable table is safe: it's only observed by [$ dune ocaml merlin] *)
    lazy
      (match Persistent.load file with
       | None -> Path.Build.Table.create 128
       | Some t -> t)
  ;;

  let dump () =
    if !needs_dumping && Path.build_dir_exists ()
    then (
      needs_dumping := false;
      Persistent.dump file (Lazy.force t))
  ;;

  let () = At_exit.at_exit_ignore Dune_trace.at_exit dump

  let destinations =
    lazy
      (Path.Build.Table.foldi
         (Lazy.force t)
         ~init:Path.Build.Map.empty
         ~f:(fun dst src acc -> Path.Build.Map.add_multi acc src dst)
       |> Path.Build.Map.map ~f:(List.sort ~compare:Path.Build.compare))
  ;;

  let follow =
    let rec loop destinations visited acc = function
      | [] -> List.rev acc
      | path :: rest ->
        if Path.Build.Set.mem visited path
        then loop destinations visited acc rest
        else (
          let visited = Path.Build.Set.add visited path in
          let rest =
            match Path.Build.Map.find destinations path with
            | None -> rest
            | Some paths -> paths @ rest
          in
          loop destinations visited (path :: acc) rest)
    in
    fun path ->
      let destinations = Lazy.force destinations in
      let paths = Path.Build.Map.find destinations path |> Option.value ~default:[] in
      loop destinations (Path.Build.Set.singleton path) [] paths
  ;;

  let set ~src ~dst =
    let t = Lazy.force t in
    let _, src = Path.Build.split_sandbox_root src in
    let _, dst = Path.Build.split_sandbox_root dst in
    needs_dumping := true;
    (* A destination can change sources without retaining its old mapping. *)
    Path.Build.Table.set t dst src
  ;;
end

let line_directive ~filename:fn ~line_number =
  let directive = if Foreign_language.has_foreign_extension ~fn then "line" else "" in
  sprintf "#%s %d %S\n" directive line_number fn
;;

module Spec = struct
  type merlin =
    | Yes
    | No

  let bool_of_merlin = function
    | Yes -> true
    | No -> false
  ;;

  type ('path, 'target) t = 'path * 'target * merlin

  let name = "copy-line-directive"
  let version = 3
  let runs_process = false
  let can_run_in_action_runner = false
  let bimap (src, dst, merlin) f g = f src, g dst, merlin
  let is_useful_to_memoize = true

  let encode (src, dst, merlin) path target : Sexp.t =
    List [ path src; target dst; Atom (Bool.to_string (bool_of_merlin merlin)) ]
  ;;

  let action (src, dst, merlin) ~ectx:_ ~eenv:_ =
    Io.with_file_in src ~f:(fun ic ->
      Path.build dst
      |> Io.with_file_out ~f:(fun oc ->
        let fn = Path.drop_optional_build_context_maybe_sandboxed src in
        output_string oc (line_directive ~filename:(Path.to_string fn) ~line_number:1);
        Io.copy_channels ic oc));
    (match merlin with
     | No -> ()
     | Yes -> Path.as_in_build_dir src |> Option.iter ~f:(fun src -> DB.set ~src ~dst));
    Fiber.return ()
  ;;
end

module A = Action_ext.Make (Spec)

let action (context : Context.t) ~src ~dst =
  A.action (src, dst, if Context.merlin context then Spec.Yes else No)
;;

let builder context ~src ~dst =
  let open Action_builder.O in
  Action_builder.with_file_targets
    ~file_targets:[ dst ]
    (Action_builder.path src
     >>> Action_builder.return (Action.Full.make (action context ~src ~dst)))
;;
