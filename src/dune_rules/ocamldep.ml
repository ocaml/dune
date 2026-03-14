open Import
open Memo.O

let fold_lines contents ~init ~f =
  let len = String.length contents in
  let rec loop ~line_start ~last_is_cr acc i =
    if i = len
    then
      if i = line_start || (i = line_start + 1 && last_is_cr)
      then acc
      else (
        let line_len = if last_is_cr then i - line_start - 1 else i - line_start in
        f acc ~line_start ~line_len)
    else (
      match String.unsafe_get contents i with
      | '\r' -> loop ~line_start ~last_is_cr:true acc (i + 1)
      | '\n' ->
        let line_len = if last_is_cr then i - line_start - 1 else i - line_start in
        let acc = f acc ~line_start ~line_len in
        let next = i + 1 in
        loop ~line_start:next ~last_is_cr:false acc next
      | _ -> loop ~line_start ~last_is_cr:false acc (i + 1))
  in
  loop ~line_start:0 ~last_is_cr:false init 0
;;

module Merge_files_into = struct
  module Spec = struct
    type ('src, 'dst) t =
      { transitive : 'src list
      ; immediate : Module_name.Unique.t list
      ; target : 'dst
      }

    let name = "merge_files_into"
    let version = 2
    let is_useful_to ~memoize:_ = true

    let bimap t path target =
      { t with transitive = List.map t.transitive ~f:path; target = target t.target }
    ;;

    let encode
          (type src dst)
          ({ transitive; immediate; target } : (src, dst) t)
          (input : src -> Sexp.t)
          (output : dst -> Sexp.t)
      : Sexp.t
      =
      List
        [ List (List.map transitive ~f:input)
        ; List
            (List.map ~f:(fun s -> Sexp.Atom (Module_name.Unique.to_string s)) immediate)
        ; output target
        ]
    ;;

    let add_modules_from_file set source_path =
      let contents = Io.read_file ~binary:false source_path in
      fold_lines contents ~init:set ~f:(fun set ~line_start ~line_len ->
        let module_name =
          String.sub contents ~pos:line_start ~len:line_len
          |> Module_name.Unique.of_string
        in
        Module_name.Unique.Set.add set module_name)
    ;;

    let action { transitive; immediate; target } ~ectx:_ ~eenv:_ =
      Async.async (fun () ->
        List.fold_left
          transitive
          ~init:(Module_name.Unique.Set.of_list immediate)
          ~f:add_modules_from_file
        |> Module_name.Unique.Set.to_list_map ~f:Module_name.Unique.to_string
        |> Io.write_lines (Path.build target))
    ;;
  end

  module Action = Action_ext.Make (Spec)

  let action ~transitive ~immediate ~target =
    Action.action { transitive; immediate; target }
  ;;
end

let fold_blank_separated_words contents ~start ~stop ~init ~f =
  let rec skip_blanks acc i =
    if i = stop
    then acc
    else (
      match String.unsafe_get contents i with
      | ' ' | '\t' -> skip_blanks acc (i + 1)
      | _ -> parse_word acc ~word_start:i (i + 1))
  and parse_word acc ~word_start i =
    if i = stop
    then f acc ~word_start ~word_len:(i - word_start)
    else (
      match String.unsafe_get contents i with
      | ' ' | '\t' ->
        let acc = f acc ~word_start ~word_len:(i - word_start) in
        skip_blanks acc (i + 1)
      | _ -> parse_word acc ~word_start (i + 1))
  in
  skip_blanks init start
;;

let invalid_ocamldep_output ~file contents =
  let lines = String.split_lines contents in
  User_error.raise
    [ Pp.textf
        "ocamldep returned unexpected output for %s:"
        (Path.to_string_maybe_quoted file)
    ; Pp.vbox
        (Pp.concat_map lines ~sep:Pp.cut ~f:(fun line ->
           Pp.seq (Pp.verbatim "> ") (Pp.verbatim line)))
    ]
;;

let basename_matches contents ~line_start ~colon basename =
  let basename_len = String.length basename in
  let rec find_basename_start i =
    if i = line_start
    then line_start
    else if Path.is_dir_sep (String.unsafe_get contents (i - 1))
    then i
    else find_basename_start (i - 1)
  in
  let basename_start = find_basename_start colon in
  colon - basename_start = basename_len
  &&
  let rec loop i =
    i = basename_len
    || (String.unsafe_get contents (basename_start + i) = String.unsafe_get basename i
        && loop (i + 1))
  in
  loop 0
;;

let parse_ocamldep_output_exn ~file contents ~init ~f =
  let line =
    fold_lines contents ~init:None ~f:(fun line ~line_start ~line_len ->
      match line with
      | None -> Some (line_start, line_len)
      | Some _ -> invalid_ocamldep_output ~file contents)
  in
  let line_start, line_len =
    match line with
    | Some line -> line
    | None -> invalid_ocamldep_output ~file contents
  in
  let line_end = line_start + line_len in
  let colon =
    let rec loop i =
      if i = line_end
      then invalid_ocamldep_output ~file contents
      else if String.unsafe_get contents i = ':'
      then i
      else loop (i + 1)
    in
    loop line_start
  in
  if not (basename_matches contents ~line_start ~colon (Path.basename file))
  then invalid_ocamldep_output ~file contents;
  fold_blank_separated_words contents ~start:(colon + 1) ~stop:line_end ~init ~f
;;

let raise_parent_cycle_error ~dir ~(unit : Module.t) m =
  User_error.raise
    [ Pp.textf
        "Module %s in directory %s depends on %s."
        (Module_name.to_string (Module.name unit))
        (Path.to_string_maybe_quoted (Path.build dir))
        (Module_name.to_string m)
    ; Pp.textf "This doesn't make sense to me."
    ; Pp.nop
    ; Pp.textf
        "%s is the main module of the library and is the only module exposed outside of \
         the library. Consequently, it should be the one depending on all the other \
         modules in the library."
        (Module_name.to_string m)
    ]
;;

let parse_output ~file contents =
  parse_ocamldep_output_exn ~file contents ~init:[] ~f:(fun acc ~word_start ~word_len ->
    String.sub contents ~pos:word_start ~len:word_len :: acc)
  |> List.rev
;;

let parse_module_names ~file ~dir ~(unit : Module.t) ~modules contents =
  let deps =
    parse_ocamldep_output_exn ~file contents ~init:[] ~f:(fun acc ~word_start ~word_len ->
      let module_name =
        String.sub contents ~pos:word_start ~len:word_len |> Module_name.of_checked_string
      in
      match Modules.With_vlib.find_dep modules ~of_:unit module_name with
      | Ok deps -> List.rev_append deps acc
      | Error `Parent_cycle -> raise_parent_cycle_error ~dir ~unit module_name)
  in
  List.rev_append deps (Modules.With_vlib.implicit_deps modules ~of_:unit)
;;

let parse_compilation_units ~modules contents =
  let obj_map = Modules.With_vlib.obj_map modules in
  fold_lines contents ~init:[] ~f:(fun acc ~line_start ~line_len ->
    let obj_name =
      String.sub contents ~pos:line_start ~len:line_len |> Module_name.Unique.of_string
    in
    match Module_name.Unique.Map.find obj_map obj_name with
    | None -> acc
    | Some sourced_module -> Modules.Sourced_module.to_module sourced_module :: acc)
  |> List.rev
;;

let transitive_deps =
  let transive_dep obj_dir m ~for_ =
    (match Module.kind m with
     | Root | Alias _ -> None
     | _ -> if Module.has m ~ml_kind:Intf then Some Ml_kind.Intf else Some Impl)
    |> Option.map ~f:(fun ml_kind ->
      Obj_dir.Module.dep obj_dir ~for_ (Transitive (m, ml_kind))
      |> Option.value_exn (* we already checked if it's an alias module *)
      |> Path.build)
  in
  fun obj_dir modules ~for_ -> List.filter_map modules ~f:(transive_dep obj_dir ~for_)
;;

let deps_of ~sandbox ~modules ~sctx ~dir ~obj_dir ~ml_kind ~for_ unit =
  let source = Option.value_exn (Module.source unit ~ml_kind) in
  let dep = Obj_dir.Module.dep obj_dir ~for_ in
  let ocamldep_output = dep (Immediate (unit, ml_kind)) |> Option.value_exn in
  let* () =
    let context = Super_context.context sctx in
    let ocamldep =
      (let+ ocaml = Context.ocaml context in
       ocaml.ocamldep)
      |> Action_builder.of_memo
    in
    Super_context.add_rule
      sctx
      ~dir
      (let open Action_builder.With_targets.O in
       let flags, sandbox =
         Module.pp_flags unit |> Option.value ~default:(Action_builder.return [], sandbox)
       in
       Command.run_dyn_prog
         ocamldep
         ~dir:(Path.build (Context.build_dir context))
         ~stdout_to:ocamldep_output
         [ A "-modules"
         ; Command.Args.dyn flags
         ; Command.Ml_kind.flag ml_kind
         ; Dep (Module.File.path source)
         ]
       >>| Action.Full.add_sandbox sandbox)
  in
  let all_deps_file = dep (Transitive (unit, ml_kind)) |> Option.value_exn in
  let+ () =
    let produce_all_deps =
      let open Action_builder.O in
      (let+ transitive, immediate =
         (let+ immediate_deps =
            Path.build ocamldep_output
            |> Action_builder.contents
            >>| parse_module_names ~file:(Module.File.path source) ~dir ~unit ~modules
          in
          let transitive_deps = transitive_deps obj_dir immediate_deps ~for_ in
          let immediate_deps = List.map immediate_deps ~f:Module.obj_name in
          (transitive_deps, immediate_deps), transitive_deps)
         |> Action_builder.dyn_paths
       in
       Merge_files_into.action ~transitive ~immediate ~target:all_deps_file)
      |> Action_builder.with_file_targets ~file_targets:[ all_deps_file ]
    in
    Action_builder.With_targets.map ~f:Action.Full.make produce_all_deps
    |> Super_context.add_rule sctx ~dir
  in
  let all_deps_file = Path.build all_deps_file in
  Action_builder.contents all_deps_file
  |> Action_builder.map ~f:(parse_compilation_units ~modules)
  |> Action_builder.memoize (Path.to_string all_deps_file)
;;

let read_deps_of ~obj_dir ~modules ~ml_kind ~for_ unit =
  let all_deps_file =
    Obj_dir.Module.dep obj_dir ~for_ (Transitive (unit, ml_kind)) |> Option.value_exn
  in
  Action_builder.contents (Path.build all_deps_file)
  |> Action_builder.map ~f:(parse_compilation_units ~modules)
  |> Action_builder.memoize (Path.Build.to_string all_deps_file)
;;

let read_immediate_deps_of ~obj_dir ~modules ~ml_kind ~for_ unit =
  match Module.source ~ml_kind unit with
  | None -> Action_builder.return []
  | Some source ->
    let ocamldep_output =
      Obj_dir.Module.dep obj_dir ~for_ (Immediate (unit, ml_kind)) |> Option.value_exn
    in
    Action_builder.contents (Path.build ocamldep_output)
    |> Action_builder.map ~f:(fun contents ->
      parse_module_names
        ~file:(Module.File.path source)
        ~dir:(Obj_dir.dir obj_dir)
        ~unit
        ~modules
        contents)
    |> Action_builder.memoize (Path.Build.to_string ocamldep_output)
;;
