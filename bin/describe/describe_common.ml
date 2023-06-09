open Stdune
open Import

(* This command is not yet versioned, but some people are using it in
   non-released tools. If you change the format of the output, please contact:

   - rotor people for "describe workspace"

   - duniverse people for "describe opam-files" *)

module Descr = struct
  type options =
    { with_deps : bool
    ; with_pps : bool
    }

  let dyn_path (p : Path.t) : Dyn.t = String (Path.to_string p)

  module Mod_deps = struct
    type t =
      { for_intf : Dune_rules.Module_name.t list
      ; for_impl : Dune_rules.Module_name.t list
      }

    let to_dyn { for_intf; for_impl } =
      let open Dyn in
      record
        [ ("for_intf", list Dune_rules.Module_name.to_dyn for_intf)
        ; ("for_impl", list Dune_rules.Module_name.to_dyn for_impl)
        ]
  end

  module Mod = struct
    type t =
      { name : Dune_rules.Module_name.t
      ; impl : Path.t option
      ; intf : Path.t option
      ; cmt : Path.t option
      ; cmti : Path.t option
      ; module_deps : Mod_deps.t
      }

    let to_dyn options { name; impl; intf; cmt; cmti; module_deps } : Dyn.t =
      let open Dyn in
      let optional_fields =
        let module_deps =
          if options.with_deps then
            Some ("module_deps", Mod_deps.to_dyn module_deps)
          else None
        in
        (* we build a list of options, that is later filtered, so that adding
           new optional fields in the future can be done easily *)
        match module_deps with
        | None -> []
        | Some module_deps -> [ module_deps ]
      in
      record
      @@ [ ("name", Dune_rules.Module_name.to_dyn name)
         ; ("impl", option dyn_path impl)
         ; ("intf", option dyn_path intf)
         ; ("cmt", option dyn_path cmt)
         ; ("cmti", option dyn_path cmti)
         ]
      @ optional_fields
  end

  module Exe = struct
    type t =
      { names : string list
      ; requires : Dune_digest.t list
      ; modules : Mod.t list
      ; include_dirs : Path.t list
      }

    let map_path t ~f = { t with include_dirs = List.map ~f t.include_dirs }

    let to_dyn options { names; requires; modules; include_dirs } : Dyn.t =
      let open Dyn in
      record
        [ ("names", List (List.map ~f:(fun name -> String name) names))
        ; ("requires", Dyn.(list string) (List.map ~f:Digest.to_string requires))
        ; ("modules", list (Mod.to_dyn options) modules)
        ; ("include_dirs", list dyn_path include_dirs)
        ]
  end

  module Lib = struct
    type t =
      { name : Lib_name.t
      ; uid : Dune_digest.t
      ; local : bool
      ; requires : Dune_digest.t list
      ; source_dir : Path.t
      ; modules : Mod.t list
      ; include_dirs : Path.t list
      }

    let map_path t ~f =
      { t with
        source_dir = f t.source_dir
      ; include_dirs = List.map ~f t.include_dirs
      }

    let to_dyn options
        { name; uid; local; requires; source_dir; modules; include_dirs } :
        Dyn.t =
      let open Dyn in
      record
        [ ("name", Lib_name.to_dyn name)
        ; ("uid", String (Digest.to_string uid))
        ; ("local", Bool local)
        ; ("requires", (list string) (List.map ~f:Digest.to_string requires))
        ; ("source_dir", dyn_path source_dir)
        ; ("modules", list (Mod.to_dyn options) modules)
        ; ("include_dirs", (list dyn_path) include_dirs)
        ]
  end

  module Item = struct
    type t =
      | Executables of Exe.t
      | Library of Lib.t
      | Root of Path.t
      | Build_context of Path.t

    let map_path t ~f =
      match t with
      | Executables exe -> Executables (Exe.map_path exe ~f)
      | Library lib -> Library (Lib.map_path lib ~f)
      | Root r -> Root (f r)
      | Build_context c -> Build_context (f c)

    let to_dyn options : t -> Dyn.t = function
      | Executables exe_descr ->
        Variant ("executables", [ Exe.to_dyn options exe_descr ])
      | Library lib_descr ->
        Variant ("library", [ Lib.to_dyn options lib_descr ])
      | Root root ->
        Variant ("root", [ String (Path.to_absolute_filename root) ])
      | Build_context build_ctxt ->
        Variant ("build_context", [ String (Path.to_string build_ctxt) ])
  end

  module Workspace = struct
    type t = Item.t list

    let to_dyn options (items : t) : Dyn.t =
      Dyn.list (Item.to_dyn options) items
  end
end

module Format = struct
  type t =
    | Sexp
    | Csexp

  let all = [ ("sexp", Sexp); ("csexp", Csexp) ]

  let arg =
    let doc = Printf.sprintf "$(docv) must be %s" (Arg.doc_alts_enum all) in
    Arg.(value & opt (enum all) Sexp & info [ "format" ] ~docv:"FORMAT" ~doc)

  let print_as_sexp dyn =
    let rec dune_lang_of_sexp : Sexp.t -> Dune_lang.t = function
      | Atom s -> Dune_lang.atom_or_quoted_string s
      | List l -> List (List.map l ~f:dune_lang_of_sexp)
    in
    let cst =
      dyn |> Sexp.of_dyn |> dune_lang_of_sexp
      |> Dune_lang.Ast.add_loc ~loc:Loc.none
      |> Dune_lang.Cst.concrete
    in
    let version = Dune_lang.Syntax.greatest_supported_version Stanza.syntax in
    Pp.to_fmt Stdlib.Format.std_formatter
      (Dune_lang.Format.pp_top_sexps ~version [ cst ])

  let print_dyn t dyn =
    match t with
    | Csexp -> Csexp.to_channel stdout (Sexp.of_dyn dyn)
    | Sexp -> print_as_sexp dyn
end
