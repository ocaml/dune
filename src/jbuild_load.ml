open Import
open Jbuild

let filter_stanzas ~ignore_promoted_rules stanzas =
  if ignore_promoted_rules then
    List.filter stanzas ~f:(function
      | Stanza.Rule { mode = Promote; _ } -> false
      | _ -> true)
  else
    stanzas

module Jbuilds = struct
  type script =
    { dir   : Path.t
    ; scope : Scope.t
    }

  type one =
    | Literal of (Path.t * Scope.t * Stanza.t list)
    | Script of script

  type t =
    { jbuilds               : one list
    ; ignore_promoted_rules : bool
    }

  let generated_jbuilds_dir = Path.(relative root) "_build/.jbuilds"

  let ensure_parent_dir_exists path =
    match Path.kind path with
    | Local path -> Path.Local.ensure_parent_directory_exists path
    | External _ -> ()

  let extract_requires ~fname str =
    let rec loop n lines acc =
      match lines with
      | [] -> acc
      | line :: lines ->
        let acc =
          match Scanf.sscanf line "#require %S" (fun x -> x) with
          | exception _ -> acc
          | s ->
            match String.split s ~on:',' with
            | [] -> acc
            | ["unix"] as l -> l
            | _ ->
              let start =
                { Lexing.
                  pos_fname = fname
                ; pos_lnum  = n
                ; pos_cnum  = 0
                ; pos_bol   = 0
                }
              in
              Loc.fail
                { start; stop = { start with pos_cnum = String.length line } }
                "Using libraries other that \"unix\" is not supported.\n\
                 See the manual for details.";
        in
        loop (n + 1) lines acc
    in
    loop 1 (String.split str ~on:'\n') []

  let create_plugin_wrapper (context : Context.t) ~exec_dir ~plugin ~wrapper ~target =
    let plugin = Path.to_string plugin in
    let plugin_contents = Io.read_file plugin in
    Io.with_file_out (Path.to_string wrapper) ~f:(fun oc ->
      Printf.fprintf oc {|
let () =
  Hashtbl.add Toploop.directive_table "require" (Toploop.Directive_string ignore);
  Hashtbl.add Toploop.directive_table "use" (Toploop.Directive_string (fun _ ->
    failwith "#use is not allowed inside jbuild in OCaml syntax"));
  Hashtbl.add Toploop.directive_table "use_mod" (Toploop.Directive_string (fun _ ->
    failwith "#use is not allowed inside jbuild in OCaml syntax"))

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
  end
end
# 1 %S
%s|}
        context.name
        context.version_string
        (Ocamlc_config.ocaml_value context.ocamlc_config)
        (Path.reach ~from:exec_dir target)
        plugin plugin_contents);
    extract_requires ~fname:plugin plugin_contents

  let eval { jbuilds; ignore_promoted_rules } ~(context : Context.t) =
    let open Fiber.O in
    let static, dynamic =
      List.partition_map jbuilds ~f:(function
        | Literal x -> Inl x
        | Script  x -> Inr x)
    in
    Fiber.parallel_map dynamic ~f:(fun { dir; scope } ->
      let file = Path.relative dir "jbuild" in
      let generated_jbuild =
        Path.append (Path.relative generated_jbuilds_dir context.name) file
      in
      let wrapper = Path.extend_basename generated_jbuild ~suffix:".ml" in
      ensure_parent_dir_exists generated_jbuild;
      let requires =
        create_plugin_wrapper context ~exec_dir:dir ~plugin:file ~wrapper
          ~target:generated_jbuild
      in
      let context = Option.value context.for_host ~default:context in
      let pkgs =
        let required_by = [With_required_by.Entry.jbuild_file_in ~dir] in
        List.map requires ~f:(Findlib.find_exn context.findlib ~required_by)
        |> Findlib.closure ~required_by ~local_public_libs:String_map.empty
      in
      let includes =
        List.fold_left pkgs ~init:Path.Set.empty ~f:(fun acc pkg ->
          Path.Set.add (Findlib.Package.dir pkg) acc)
        |> Path.Set.elements
        |> List.concat_map ~f:(fun path ->
          [ "-I"; Path.to_string path ])
      in
      let cmas =
        List.concat_map pkgs ~f:(fun pkg -> Findlib.Package.archives pkg Byte)
      in
      let args =
        List.concat
          [ [ "-I"; "+compiler-libs" ]
          ; includes
          ; List.map cmas ~f:(Path.reach ~from:dir)
          ; [ Path.to_absolute_filename wrapper ]
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
      Process.run Strict ~dir:(Path.to_string dir) ~env:context.env
        (Path.to_string context.ocaml)
        args
      >>= fun () ->
      if not (Path.exists generated_jbuild) then
        die "@{<error>Error:@} %s failed to produce a valid jbuild file.\n\
             Did you forgot to call [Jbuild_plugin.V*.send]?"
          (Path.to_string file);
      let sexps = Sexp.load ~fname:(Path.to_string generated_jbuild) ~mode:Many in
      Fiber.return (dir, scope, Stanzas.parse scope sexps ~file:generated_jbuild
                                |> filter_stanzas ~ignore_promoted_rules))
    >>| fun dynamic ->
    static @ dynamic
end

type conf =
  { file_tree : File_tree.t
  ; jbuilds   : Jbuilds.t
  ; packages  : Package.t String_map.t
  ; scopes    : Scope.t list
  }

let load ~dir ~scope ~ignore_promoted_rules =
  let file = Path.relative dir "jbuild" in
  match Sexp.load_many_or_ocaml_script (Path.to_string file) with
  | Sexps sexps ->
    Jbuilds.Literal (dir, scope,
                     Stanzas.parse scope sexps ~file
                     |> filter_stanzas ~ignore_promoted_rules)
  | Ocaml_script ->
    Script { dir; scope }

let load ?extra_ignored_subtrees ?(ignore_promoted_rules=false) () =
  let ftree = File_tree.load Path.root ?extra_ignored_subtrees in
  let packages =
    File_tree.fold ftree ~traverse_ignored_dirs:false ~init:[] ~f:(fun dir pkgs ->
      let path = File_tree.Dir.path dir in
      let files = File_tree.Dir.files dir in
      String_set.fold files ~init:pkgs ~f:(fun fn acc ->
        match Filename.split_extension fn with
        | (pkg, ".opam") when pkg <> "" ->
          let version_from_opam_file =
            let opam = Opam_file.load (Path.relative path fn |> Path.to_string) in
            match Opam_file.get_field opam "version" with
            | Some (String (_, s)) -> Some s
            | _ -> None
          in
          (pkg,
           { Package. name = pkg
           ; path
           ; version_from_opam_file
           }) :: acc
        | _ -> acc))
  in
  let packages =
    String_map.of_alist_multi packages
    |> String_map.mapi ~f:(fun name pkgs ->
      match pkgs with
      | [pkg] -> pkg
      | _ ->
        die "Too many opam files for package %S:\n%s"
          name
          (String.concat ~sep:"\n"
             (List.map pkgs ~f:(fun pkg ->
                sprintf "- %s" (Path.to_string (Package.opam_file pkg))))))
  in
  let scopes =
    String_map.values packages
    |> List.map ~f:(fun pkg -> (pkg.Package.path, pkg))
    |> Path.Map.of_alist_multi
    |> Path.Map.map ~f:Scope.make
  in
  let scopes =
    if Path.Map.mem Path.root scopes then
      scopes
    else
      Path.Map.add scopes ~key:Path.root ~data:Scope.empty
  in
  let rec walk dir jbuilds scope =
    if File_tree.Dir.ignored dir then
      jbuilds
    else begin
      let path = File_tree.Dir.path dir in
      let files = File_tree.Dir.files dir in
      let sub_dirs = File_tree.Dir.sub_dirs dir in
      let scope = Path.Map.find_default path scopes ~default:scope in
      let jbuilds =
        if String_set.mem "jbuild" files then
          let jbuild = load ~dir:path ~scope ~ignore_promoted_rules in
          jbuild :: jbuilds
        else
          jbuilds
      in
      String_map.fold sub_dirs ~init:jbuilds
        ~f:(fun ~key:_ ~data:dir jbuilds ->
          walk dir jbuilds scope)
    end
  in
  let jbuilds = walk (File_tree.root ftree) [] Scope.empty in
  { file_tree = ftree
  ; jbuilds = { jbuilds; ignore_promoted_rules }
  ; packages
  ; scopes = Path.Map.values scopes
  }
