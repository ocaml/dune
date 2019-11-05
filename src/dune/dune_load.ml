open! Stdune
open Import

module Dune_file = struct
  type t =
    { dir : Path.Source.t
    ; project : Dune_project.t
    ; stanzas : Dune_file.Stanzas.t
    }

  let parse sexps ~dir ~file ~project =
    let stanzas = Dune_file.Stanzas.parse ~file project sexps in
    let stanzas =
      if !Clflags.ignore_promoted_rules then
        List.filter stanzas ~f:(function
          | Dune_file.Rule { mode = Rule.Mode.Promote { only = None; _ }; _ }
          | Dune_file.Menhir.T
              { mode = Rule.Mode.Promote { only = None; _ }; _ } ->
            false
          | _ -> true)
      else
        stanzas
    in
    { dir; project; stanzas }

  let rec fold_stanzas l ~init ~f =
    match l with
    | [] -> init
    | t :: l -> inner_fold t t.stanzas l ~init ~f

  and inner_fold t inner_list l ~init ~f =
    match inner_list with
    | [] -> fold_stanzas l ~init ~f
    | x :: inner_list -> inner_fold t inner_list l ~init:(f t x init) ~f
end

module Dune_files = struct
  type script =
    { dir : Path.Source.t
    ; file : Path.Source.t
    ; project : Dune_project.t
    }

  type one =
    | Literal of Dune_file.t
    | Script of script

  type t = one list

  let generated_dune_files_dir = Path.Build.relative Path.Build.root ".dune"

  let ensure_parent_dir_exists path =
    Path.build path |> Path.parent |> Option.iter ~f:Path.mkdir_p

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
    let plugin_contents = Io.read_file plugin in
    Io.with_file_out (Path.build wrapper) ~f:(fun oc ->
        let ocamlc_config =
          let vars =
            Ocaml_config.to_list context.ocaml_config
            |> List.map ~f:(fun (k, v) -> (k, Ocaml_config.Value.to_string v))
          in
          let longest = String.longest_map vars ~f:fst in
          List.map vars ~f:(fun (k, v) ->
              sprintf "%-*S , %S" (longest + 2) k v)
          |> String.concat ~sep:"\n      ; "
        in
        Printf.fprintf oc
          {|
let () =
  Hashtbl.add Toploop.directive_table "require" (Toploop.Directive_string ignore);
  Hashtbl.add Toploop.directive_table "use" (Toploop.Directive_string (fun _ ->
    failwith "#use is not allowed inside a dune file in OCaml syntax"));
  Hashtbl.add Toploop.directive_table "use_mod" (Toploop.Directive_string (fun _ ->
    failwith "#use is not allowed inside a dune file in OCaml syntax"))

module Jbuild_plugin = struct
  module V1 = struct
    let context       = %S
    let ocaml_version = %S

    let ocamlc_config =
      [ %s
      ]

    let send s =
      let oc = open_out_bin %S in
      output_string oc s;
      close_out oc

    let run_and_read_lines cmd =
      let tmp_fname = Filename.temp_file "dune" ".output" in
      at_exit (fun () -> Sys.remove tmp_fname);
      let n =
        Printf.ksprintf Sys.command "%%s > %%s" cmd (Filename.quote tmp_fname)
      in
      let rec loop ic acc =
        match input_line ic with
        | exception End_of_file -> close_in ic; List.rev acc
        | line -> loop ic (line :: acc)
      in
      let output = loop (open_in tmp_fname) [] in
      if n = 0 then
        output
      else begin
        Printf.ksprintf failwith
          "Command failed: %%s\n\
           Exit code: %%d\n\
           Output:\n\
           %%s"
          cmd n (String.concat "\n" output)
      end
  end
end
# 1 %S
%s|}
          (Context_name.to_string context.name)
          context.version_string ocamlc_config
          (Path.reach ~from:exec_dir (Path.build target))
          (Path.to_string plugin) plugin_contents);
    check_no_requires plugin plugin_contents

  let eval dune_files ~(context : Context.t) =
    let open Fiber.O in
    let static, dynamic =
      List.partition_map dune_files ~f:(function
        | Literal x -> Left x
        | Script x -> Right x)
    in
    Fiber.parallel_map dynamic ~f:(fun { dir; file; project } ->
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
        create_plugin_wrapper context ~exec_dir:(Path.source dir)
          ~plugin:(Path.source file) ~wrapper ~target:generated_dune_file;
        let context = Option.value context.for_host ~default:context in
        let args =
          List.concat
            [ [ "-I"; "+compiler-libs" ]
            ; [ Path.to_absolute_filename (Path.build wrapper) ]
            ]
        in
        let* () =
          Process.run Strict ~dir:(Path.source dir) ~env:context.env
            context.ocaml args
        in
        if not (Path.exists (Path.build generated_dune_file)) then
          User_error.raise
            [ Pp.textf "%s failed to produce a valid dune_file file."
                (Path.Source.to_string_maybe_quoted file)
            ; Pp.textf "Did you forgot to call [Jbuild_plugin.V*.send]?"
            ];
        Fiber.return
          ( Dune_lang.Parser.load (Path.build generated_dune_file) ~mode:Many
          |> Dune_file.parse ~dir ~file ~project ))
    >>| fun dynamic -> static @ dynamic
end

type conf =
  { dune_files : Dune_files.t
  ; packages : Package.t Package.Name.Map.t
  ; projects : Dune_project.t list
  }

let interpret ~dir ~project ~(dune_file : File_tree.Dune_file.t) =
  match dune_file with
  | Plain p ->
    let dune_file =
      Dune_files.Literal (Dune_file.parse p.sexps ~dir ~file:p.path ~project)
    in
    p.sexps <- [];
    dune_file
  | Ocaml_script file -> Script { dir; project; file }

let load ~ancestor_vcs () =
  File_tree.init Path.Source.root ~ancestor_vcs
    ~recognize_jbuilder_projects:false;
  let projects =
    File_tree.fold_with_progress
      ~traverse:{ data_only = false; vendored = true; normal = true } ~init:[]
      ~f:(fun dir acc ->
        let p = File_tree.Dir.project dir in
        if Path.Source.equal (File_tree.Dir.path dir) (Dune_project.root p)
        then
          p :: acc
        else
          acc)
  in
  let packages =
    List.fold_left projects ~init:Package.Name.Map.empty
      ~f:(fun acc (p : Dune_project.t) ->
        Package.Name.Map.merge acc (Dune_project.packages p)
          ~f:(fun name a b ->
            match (a, b) with
            | None, None -> None
            | None, Some _ -> b
            | Some _, None -> a
            | Some a, Some b ->
              User_error.raise
                [ Pp.textf "Too many opam files for package %S:"
                    (Package.Name.to_string name)
                ; Pp.textf "- %s"
                    (Path.Source.to_string_maybe_quoted (Package.opam_file a))
                ; Pp.textf "- %s"
                    (Path.Source.to_string_maybe_quoted (Package.opam_file b))
                ]))
  in
  let rec walk dir dune_files =
    if File_tree.Dir.ignored dir then
      dune_files
    else
      let path = File_tree.Dir.path dir in
      let sub_dirs = File_tree.Dir.sub_dirs dir in
      let project = File_tree.Dir.project dir in
      let dune_files =
        match File_tree.Dir.dune_file dir with
        | None -> dune_files
        | Some dune_file ->
          let dune_file = interpret ~dir:path ~project ~dune_file in
          dune_file :: dune_files
      in
      String.Map.fold sub_dirs ~init:dune_files ~f:walk
  in
  let dune_files = walk (File_tree.root ()) [] in
  { dune_files; packages; projects }
