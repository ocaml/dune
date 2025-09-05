open Import

module DB = struct
  (* Needed to tell resolve the configuration of sources merlin gives us.

     This is all ugly and doesn't work well for watch mode, but it's better
     than the old hack. It's temporary until we have something RPC based.
  *)
  module Persistent = Dune_util.Persistent.Make (struct
      type nonrec t = Path.Build.t Path.Build.Table.t

      let name = "COPY-LINE-DIRECTIVE-MAP"
      let version = 2
      let to_dyn = Path.Build.Table.to_dyn Path.Build.to_dyn
      let test_example () = Path.Build.Table.create 1
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

  let () = at_exit dump

  let rec follow_while path ~f =
    let t = Lazy.force t in
    match Path.Build.Table.find t path with
    | None -> None
    | Some p ->
      (match f p with
       | None -> follow_while p ~f
       | Some p -> Some p)
  ;;

  let set ~src ~dst =
    let t = Lazy.force t in
    needs_dumping := true;
    Path.Build.Table.set t src dst
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
  let version = 2
  let bimap (src, dst, merlin) f g = f src, g dst, merlin
  let is_useful_to ~memoize = memoize

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
