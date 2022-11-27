open Import
open Memo.O

module Jbuild_plugin : sig
  val create_plugin_wrapper :
       Context.t
    -> exec_dir:Path.t
    -> plugin:Path.Outside_build_dir.t
    -> wrapper:Path.Build.t
    -> target:Path.Build.t
    -> unit Memo.t
end = struct
  let replace_in_template =
    let template =
      lazy
        (let marker name =
           let open Re in
           [ str "(*$"; rep space; str name; rep space; str "$*)" ]
           |> seq |> Re.mark
         in
         let mark_start, marker_start = marker "begin_vars" in
         let mark_end, marker_end = marker "end_vars" in
         let markers = Re.alt [ marker_start; marker_end ] in
         let invalid_template stage =
           Code_error.raise
             "Jbuild_plugin.replace_in_template: invalid template"
             [ ("stage", Dyn.string stage) ]
         in
         let rec parse1 = function
           | `Text s :: xs -> parse2 s xs
           | xs -> parse2 "" xs
         and parse2 prefix = function
           | `Delim ds :: `Text _ :: `Delim de :: xs
             when Re.Mark.test ds mark_start && Re.Mark.test de mark_end ->
             parse3 prefix xs
           | _ -> invalid_template "parse2"
         and parse3 prefix = function
           | [] -> (prefix, "")
           | [ `Text suffix ] -> (prefix, suffix)
           | _ -> invalid_template "parse3"
         in
         let tokens =
           Re.split_full (Re.compile markers) Assets.jbuild_plugin_ml
         in
         parse1 tokens)
    in
    fun t ->
      let prefix, suffix = Lazy.force template in
      sprintf "%s%s%s" prefix t suffix

  let write oc ~(context : Context.t) ~target ~exec_dir ~plugin ~plugin_contents
      =
    let ocamlc_config =
      let vars =
        Ocaml_config.to_list context.ocaml_config
        |> List.map ~f:(fun (k, v) -> (k, Ocaml_config.Value.to_string v))
      in
      let longest = String.longest_map vars ~f:fst in
      List.map vars ~f:(fun (k, v) -> sprintf "%-*S , %S" (longest + 2) k v)
      |> String.concat ~sep:"\n      ; "
    in
    let vars =
      Printf.sprintf
        {|let context = %S
        let ocaml_version = %S
        let send_target = %S
        let ocamlc_config = [ %s ]
        |}
        (Context_name.to_string context.name)
        (Ocaml_config.version_string context.ocaml_config)
        (Path.reach ~from:exec_dir (Path.build target))
        ocamlc_config
    in
    Printf.fprintf oc
      "module Jbuild_plugin : sig\n%s\nend = struct\n%s\nend\n# 1 %S\n%s"
      Assets.jbuild_plugin_mli (replace_in_template vars)
      (Path.Outside_build_dir.to_string plugin)
      plugin_contents

  let check_no_requires path str =
    List.iteri (String.split str ~on:'\n') ~f:(fun n line ->
        match Scanf.sscanf line "#require %S" (fun x -> x) with
        | Error () -> ()
        | Ok (_ : string) ->
          let loc : Loc.t =
            let start : Lexing.position =
              { pos_fname = Path.to_string path
              ; pos_lnum = n
              ; pos_cnum = 0
              ; pos_bol = 0
              }
            in
            { start; stop = { start with pos_cnum = String.length line } }
          in
          User_error.raise ~loc
            [ Pp.text "#require is no longer supported in dune files."
            ; Pp.text
                "You can use the following function instead of \
                 Unix.open_process_in:\n\n\
                \  (** Execute a command and read it's output *)\n\
                \  val run_and_read_lines : string -> string list"
            ])

  let create_plugin_wrapper (context : Context.t) ~exec_dir ~plugin ~wrapper
      ~target =
    let open Memo.O in
    let+ plugin_contents = Fs_memo.file_contents plugin in
    Io.with_file_out (Path.build wrapper) ~f:(fun oc ->
        write oc ~context ~target ~exec_dir ~plugin ~plugin_contents);
    check_no_requires (Path.outside_build_dir plugin) plugin_contents
end

module Script = struct
  type script =
    { dir : Path.Source.t
    ; file : Path.Source.t
    ; project : Dune_project.t
    }

  type t =
    { script : script
    ; from_parent : Dune_lang.Ast.t list
    }

  let generated_dune_files_dir = Path.Build.relative Path.Build.root ".dune"

  let ensure_parent_dir_exists path =
    Path.build path |> Path.parent |> Option.iter ~f:Path.mkdir_p

  let eval_one
      ((context : Context.t), { script = { dir; file; project }; from_parent })
      =
    let generated_dune_file =
      Path.Build.append_source
        (Path.Build.relative generated_dune_files_dir
           (Context_name.to_string context.name))
        file
    in
    let wrapper =
      Path.Build.extend_basename generated_dune_file ~suffix:".ml"
    in
    ensure_parent_dir_exists generated_dune_file;
    let* () =
      Jbuild_plugin.create_plugin_wrapper context ~exec_dir:(Path.source dir)
        ~plugin:(In_source_dir file) ~wrapper ~target:generated_dune_file
    in
    let context = Option.value context.for_host ~default:context in
    let args =
      List.concat
        [ [ "-I"; "+compiler-libs" ]
        ; [ Path.to_absolute_filename (Path.build wrapper) ]
        ]
    in
    let ocaml = Action.Prog.ok_exn context.ocaml in
    let* () =
      let* (_ : Memo.Run.t) = Memo.current_run () in
      Memo.of_reproducible_fiber
        (Process.run Strict ~dir:(Path.source dir) ~env:context.env ocaml args)
    in
    if not (Path.Untracked.exists (Path.build generated_dune_file)) then
      User_error.raise
        [ Pp.textf "%s failed to produce a valid dune_file file."
            (Path.Source.to_string_maybe_quoted file)
        ; Pp.textf "Did you forgot to call [Jbuild_plugin.V*.send]?"
        ];
    Path.build generated_dune_file
    |> Io.Untracked.with_lexbuf_from_file ~f:(Dune_lang.Parser.parse ~mode:Many)
    |> List.rev_append from_parent
    |> Dune_file.parse ~dir ~file:(Some file) ~project

  let eval_one =
    let module Input = struct
      type nonrec t = Context.t * t

      let equal = Tuple.T2.equal Context.equal ( == )

      let hash = Tuple.T2.hash Context.hash Poly.hash

      let to_dyn = Dyn.opaque
    end in
    let memo = Memo.create "Script.eval_one" ~input:(module Input) eval_one in
    fun ~context t -> Memo.exec memo (context, t)
end

module Dune_files = struct
  type one =
    | Literal of Dune_file.t
    | Script of Script.t

  type t = one list

  let interpret =
    let impl (dir, project, dune_file) =
      let file = Source_tree.Dune_file.path dune_file in
      let static = Source_tree.Dune_file.get_static_sexp dune_file in
      match Source_tree.Dune_file.kind dune_file with
      | Ocaml_script ->
        Memo.return
          (Script
             { script =
                 { dir
                 ; project
                 ; file =
                     (* we can't introduce ocaml syntax with [(sudir ..)] *)
                     Option.value_exn file
                 }
             ; from_parent = static
             })
      | Plain ->
        let open Memo.O in
        let+ stanzas = Dune_file.parse static ~dir ~file ~project in
        Literal stanzas
    in
    let module Input = struct
      type t = Path.Source.t * Dune_project.t * Source_tree.Dune_file.t

      let equal = Tuple.T3.equal Path.Source.equal Dune_project.equal ( == )

      let hash = Tuple.T3.hash Path.Source.hash Dune_project.hash Poly.hash

      let to_dyn = Dyn.opaque
    end in
    let memo = Memo.create "Dune_files.interpret" ~input:(module Input) impl in
    fun ~dir ~project ~(dune_file : Source_tree.Dune_file.t) ->
      Memo.exec memo (dir, project, dune_file)

  let in_dir dir =
    let source_dir = Path.Build.drop_build_context_exn dir in
    let* context = Context.DB.by_dir dir in
    let* dir = Source_tree.find_dir source_dir in
    match dir with
    | None -> Memo.return None
    | Some d -> (
      let project = Source_tree.Dir.project d in
      match Source_tree.Dir.dune_file d with
      | None ->
        let dir = Source_tree.Dir.path d in
        Memo.return (Some { Dune_file.dir; project; stanzas = [] })
      | Some dune_file -> (
        let* dune_file = interpret ~dir:source_dir ~project ~dune_file in
        match dune_file with
        | Literal dune_file -> Memo.return (Some dune_file)
        | Script script ->
          let+ dune_file = Script.eval_one ~context script in
          Some dune_file))

  let eval dune_files ~(context : Context.t) =
    let open Memo.O in
    let static, dynamic =
      List.partition_map dune_files ~f:(function
        | Literal x -> Left x
        | Script y -> Right y)
    in
    let+ dynamic = Memo.parallel_map dynamic ~f:(Script.eval_one ~context) in
    static @ dynamic
end

type conf =
  { dune_files : Dune_files.t
  ; packages : Package.t Package.Name.Map.t
  ; projects : Dune_project.t list
  }

module Projects_and_dune_files =
  Monoid.Product
    (Monoid.Appendable_list (struct
      type t = Dune_project.t
    end))
    (Monoid.Appendable_list (struct
      type t = Path.Source.t * Dune_project.t * Source_tree.Dune_file.t
    end))

module Source_tree_map_reduce =
  Source_tree.Make_map_reduce_with_progress (Memo) (Projects_and_dune_files)

let load () =
  let open Memo.O in
  let* projects, dune_files =
    let f dir : Projects_and_dune_files.t Memo.t =
      let path = Source_tree.Dir.path dir in
      let project = Source_tree.Dir.project dir in
      let projects =
        if Path.Source.equal path (Dune_project.root project) then
          Appendable_list.singleton project
        else Appendable_list.empty
      in
      let dune_files =
        match Source_tree.Dir.dune_file dir with
        | None -> Appendable_list.empty
        | Some d -> Appendable_list.singleton (path, project, d)
      in
      Memo.return (projects, dune_files)
    in
    Source_tree_map_reduce.map_reduce ~traverse:Sub_dirs.Status.Set.all ~f
  in
  let projects = Appendable_list.to_list projects in
  let packages =
    List.fold_left projects ~init:Package.Name.Map.empty
      ~f:(fun acc (p : Dune_project.t) ->
        Package.Name.Map.merge acc (Dune_project.packages p) ~f:(fun name a b ->
            Option.merge a b ~f:(fun a b ->
                User_error.raise
                  [ Pp.textf "Too many opam files for package %S:"
                      (Package.Name.to_string name)
                  ; Pp.textf "- %s"
                      (Path.Source.to_string_maybe_quoted (Package.opam_file a))
                  ; Pp.textf "- %s"
                      (Path.Source.to_string_maybe_quoted (Package.opam_file b))
                  ])))
  in
  let+ dune_files =
    Appendable_list.to_list dune_files
    |> Memo.parallel_map ~f:(fun (dir, project, dune_file) ->
           Dune_files.interpret ~dir ~project ~dune_file)
  in
  { dune_files; packages; projects }

let load =
  let memo = Memo.lazy_ load in
  fun () -> Memo.Lazy.force memo
