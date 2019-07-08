open! Stdune
open Import
open Dune_file

module Dune_file = struct
  type t =
    { dir     : Path.Source.t
    ; project : Dune_project.t
    ; stanzas : Stanzas.t
    ; kind    : Dune_lang.File_syntax.t
    }

  let parse sexps ~dir ~file ~project ~kind ~ignore_promoted_rules =
    let stanzas = Stanzas.parse ~file ~kind project sexps in
    let stanzas =
      if ignore_promoted_rules then
        List.filter stanzas ~f:(function
          | Rule { mode = Promote { only = None; _ }; _ }
          | Dune_file.Menhir.T { mode = Promote { only = None; _ }; _ } -> false
          | _ -> true)
      else
        stanzas
    in
    { dir
    ; project
    ; stanzas
    ; kind
    }

  let rec fold_stanzas l ~init ~f =
    match l with
    | [] -> init
    | t :: l -> inner_fold t t.stanzas l ~init ~f

  and inner_fold t inner_list l ~init ~f =
    match inner_list with
    | [] -> fold_stanzas l ~init ~f
    | x :: inner_list ->
      inner_fold t inner_list l ~init:(f t x init) ~f

end

module Dune_files = struct
  type script =
    { dir     : Path.Source.t
    ; file    : Path.Source.t
    ; project : Dune_project.t
    ; kind    : Dune_lang.File_syntax.t
    }

  type one =
    | Literal of Dune_file.t
    | Script  of script

  type t =
    { dune_files            : one list
    ; ignore_promoted_rules : bool
    }

  let generated_dune_files_dir = Path.Build.relative Path.Build.root ".dune"

  let ensure_parent_dir_exists path =
    Path.build path
    |> Path.parent
    |> Option.iter ~f:(Path.mkdir_p)

  type requires = No_requires | Unix

  let extract_requires path str ~kind =
    let rec loop n lines acc =
      match lines with
      | [] -> acc
      | line :: lines ->
        let acc =
          match Scanf.sscanf line "#require %S" (fun x -> x) with
          | exception _ -> acc
          | s ->
            let loc : Loc.t =
              let start : Lexing.position =
                { pos_fname = Path.to_string path
                ; pos_lnum  = n
                ; pos_cnum  = 0
                ; pos_bol   = 0
                }
              in
              { start; stop = { start with pos_cnum = String.length line } }
            in
            (match (kind : Dune_lang.File_syntax.t) with
             | Jbuild -> ()
             | Dune ->
               User_error.raise ~loc
                 [ Pp.text
                     "#require is no longer supported in dune files."
                 ; Pp.text
                     "You can use the following function instead of \
                      Unix.open_process_in:\n\
                      \n\
                     \  (** Execute a command and read it's output *)\n\
                     \  val run_and_read_lines : string -> string list"
                 ]);
            match String.split s ~on:',' with
            | [] -> acc
            | ["unix"] -> Unix
            | _ ->
              User_error.raise ~loc
                [ Pp.text
                    "Using libraries other that \"unix\" is not supported."
                ; Pp.text "See the manual for details."
                ];
        in
        loop (n + 1) lines acc
    in
    loop 1 (String.split str ~on:'\n') No_requires

  let create_plugin_wrapper (context : Context.t) ~exec_dir ~plugin ~wrapper
        ~target ~kind =
    let plugin_contents = Io.read_file plugin in
    Io.with_file_out (Path.build wrapper) ~f:(fun oc ->
      let ocamlc_config =
        let vars =
          Ocaml_config.to_list context.ocaml_config
          |> List.map ~f:(fun (k, v) -> k, Ocaml_config.Value.to_string v)
        in
        let longest = String.longest_map vars ~f:fst in
        List.map vars ~f:(fun (k, v) -> sprintf "%-*S , %S" (longest + 2) k v)
        |> String.concat ~sep:"\n      ; "
      in
      Printf.fprintf oc {|
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
        context.name
        context.version_string
        ocamlc_config
        (Path.reach ~from:exec_dir (Path.build target))
        (Path.to_string plugin) plugin_contents);
    extract_requires plugin plugin_contents ~kind

  let eval { dune_files; ignore_promoted_rules } ~(context : Context.t) =
    let open Fiber.O in
    let static, dynamic =
      List.partition_map dune_files ~f:(function
        | Literal x -> Left  x
        | Script  x -> Right x)
    in
    Fiber.parallel_map dynamic ~f:(fun { dir; file; project; kind } ->
      let generated_dune_file =
        Path.Build.append_source
          (Path.Build.relative generated_dune_files_dir context.name) file
      in
      let wrapper =
        Path.Build.extend_basename generated_dune_file ~suffix:".ml" in
      ensure_parent_dir_exists generated_dune_file;
      let requires =
        create_plugin_wrapper context
          ~exec_dir:(Path.source dir) ~plugin:(Path.source file) ~wrapper
          ~target:generated_dune_file ~kind
      in
      let context = Option.value context.for_host ~default:context in
      let cmas =
        match requires with
        | No_requires -> []
        | Unix        -> ["unix.cma"]
      in
      let args =
        List.concat
          [ [ "-I"; "+compiler-libs" ]
          ; cmas
          ; [ Path.to_absolute_filename (Path.build wrapper) ]
          ]
      in
      (* CR-someday jdimino: if we want to allow plugins to use findlib:
         {[
           let args =
             match context.toplevel_path with
             | None -> args
             | Some path -> "-I" :: Path.reach ~from:dir path :: args
           in
         ]}
      *)
      let* () =
        Process.run Strict ~dir:(Path.source dir)
          ~env:context.env context.ocaml args in
      if not (Path.exists (Path.build generated_dune_file)) then
        User_error.raise
          [ Pp.textf "%s failed to produce a valid dune_file file."
              (Path.Source.to_string_maybe_quoted file)
          ; Pp.textf "Did you forgot to call [Jbuild_plugin.V*.send]?"
          ];
      Fiber.return
        (Dune_lang.Io.load (Path.build generated_dune_file) ~mode:Many
           ~lexer:(Dune_lang.Lexer.of_syntax kind)
         |> Dune_file.parse ~dir ~file ~project ~kind ~ignore_promoted_rules))
    >>| fun dynamic ->
    static @ dynamic
end

type conf =
  { file_tree  : File_tree.t
  ; dune_files : Dune_files.t
  ; packages   : Package.t Package.Name.Map.t
  ; projects   : Dune_project.t list
  }

let interpret ~dir ~project ~ignore_promoted_rules
      ~(dune_file:File_tree.Dune_file.t) =
  match dune_file.contents with
  | Plain p ->
    let dune_file =
      Dune_files.Literal
        (Dune_file.parse p.sexps ~dir ~file:p.path
           ~project
           ~kind:dune_file.kind
           ~ignore_promoted_rules)
    in
    p.sexps <- [];
    dune_file
  | Ocaml_script file ->
    Script { dir; project; file; kind = dune_file.kind }

let load ?(ignore_promoted_rules=false) ~ancestor_vcs () =
  let ftree = File_tree.load Path.Source.root ~ancestor_vcs in
  let projects =
    File_tree.fold ftree
      ~traverse:{data_only = false; vendored = true; normal = true}
      ~init:[]
      ~f:(fun dir acc ->
        let p = File_tree.Dir.project dir in
        if Path.Source.equal
             (File_tree.Dir.path dir)
             (Dune_project.root p)
        then p :: acc
        else acc)
  in
  let packages =
    List.fold_left projects ~init:Package.Name.Map.empty
      ~f:(fun acc (p : Dune_project.t) ->
        Package.Name.Map.merge acc (Dune_project.packages p) ~f:(fun name a b ->
          match a, b with
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
    else begin
      let path = File_tree.Dir.path dir in
      let sub_dirs = File_tree.Dir.sub_dirs dir in
      let project = File_tree.Dir.project dir in
      let dune_files =
        match File_tree.Dir.dune_file dir with
        | None -> dune_files
        | Some dune_file ->
          let dune_file =
            interpret ~dir:path ~project ~ignore_promoted_rules ~dune_file
          in
          dune_file :: dune_files
      in
      String.Map.fold sub_dirs ~init:dune_files ~f:walk
    end
  in
  let dune_files = walk (File_tree.root ftree) [] in
  { file_tree = ftree
  ; dune_files = { dune_files; ignore_promoted_rules }
  ; packages
  ; projects
  }
