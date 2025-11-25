(***********************************************)
(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* (c) INRIA 2019-2024                         *)
(* (c) Emilio J. Gallego Arias 2024-2025       *)
(* (c) CNRS 2025                               *)
(***********************************************)
(* Written by: Ali Caglayan                    *)
(* Written by: Emilio Jesús Gallego Arias      *)
(* Written by: Rudi Grinberg                   *)
(* Written by: Rodolphe Lepigre                *)
(***********************************************)

open Import
open Dune_lang.Decoder

let rocq_syntax =
  Dune_lang.Syntax.create
    ~name:"rocq"
    ~desc:"Rocq Prover build language"
    [ (0, 11), `Since (3, 21) ]
;;

let get_rocq_syntax () = Dune_lang.Syntax.get_exn rocq_syntax

module Rocqpp = struct
  type t =
    { modules : Ordered_set_lang.t
    ; loc : Loc.t
    }

  let decode =
    fields
      (let+ modules = Ordered_set_lang.field "modules"
       and+ loc = loc in
       { modules; loc })
  ;;

  include Stanza.Make (struct
      type nonrec t = t

      include Poly
    end)

  let p = "rocq.pp", decode_stanza decode
end

module Buildable = struct
  type t =
    { flags : Ordered_set_lang.Unexpanded.t
    ; coq_lang_version : Dune_sexp.Syntax.Version.t
    ; mode : Rocq_mode.t option
    ; use_stdlib : bool
    ; plugins : (Loc.t * Lib_name.t) list (** ocaml libraries *)
    ; theories : (Loc.t * Rocq_lib_name.t) list (** rocq libraries *)
    ; loc : Loc.t
    }

  let merge_plugins_libraries ~plugins ~libraries =
    match plugins, libraries with
    | p, [] -> p
    | [], ls -> ls
    | _, (loc, _) :: _ ->
      User_error.raise
        ~loc
        [ Pp.text
            "Cannot both use 'plugins' and 'libraries', please remove 'libraries' as it \
             has been deprecated since version 0.5 of the Coq language. It will be \
             removed before version 1.0."
        ]
  ;;

  let decode =
    let* coq_lang_version = get_rocq_syntax () in
    let+ loc = loc
    and+ flags = Ordered_set_lang.Unexpanded.field "flags"
    and+ mode =
      field_o
        "mode"
        (Dune_lang.Syntax.since rocq_syntax (0, 3) >>> Rocq_mode.decode ~rocq_syntax)
    and+ use_stdlib =
      field
        ~default:true
        "stdlib"
        (Dune_lang.Syntax.since rocq_syntax (0, 6) >>> enum [ "yes", true; "no", false ])
    and+ libraries =
      field
        "libraries"
        ~default:[]
        (Dune_sexp.Syntax.deprecated_in
           rocq_syntax
           (0, 5)
           ~extra_info:"It has been renamed to 'plugins'."
         >>> repeat (located Lib_name.decode))
    and+ plugins = field "plugins" (repeat (located Lib_name.decode)) ~default:[]
    and+ theories =
      field
        "theories"
        (Dune_lang.Syntax.since rocq_syntax (0, 2) >>> repeat Rocq_lib_name.decode)
        ~default:[]
    in
    let plugins = merge_plugins_libraries ~plugins ~libraries in
    { flags; mode; use_stdlib; coq_lang_version; plugins; theories; loc }
  ;;
end

module Extraction = struct
  type t =
    { (* not a list of modules because we want to preserve whatever case Rocq
         uses *)
      extracted_modules : string list
    ; prelude : Loc.t * Rocq_module.Name.t
    ; buildable : Buildable.t
    }

  let ml_target_fnames t =
    List.concat_map t.extracted_modules ~f:(fun m -> [ m ^ ".ml"; m ^ ".mli" ])
  ;;

  let decode =
    fields
      (let+ extracted_modules = field "extracted_modules" (repeat string)
       and+ prelude = field "prelude" (located (string >>| Rocq_module.Name.make))
       and+ buildable = Buildable.decode in
       { prelude; extracted_modules; buildable })
  ;;

  include Stanza.Make (struct
      type nonrec t = t

      include Poly
    end)

  let p = "rocq.extraction", decode_stanza decode
end

module Theory = struct
  type t =
    { name : Loc.t * Rocq_lib_name.t
    ; package : Package.t option
    ; project : Dune_project.t
    ; synopsis : string option
    ; modules : Ordered_set_lang.t
    ; modules_flags : (Rocq_module.Name.t * Ordered_set_lang.Unexpanded.t) list option
    ; boot : bool
    ; generate_project_file : Loc.t * bool
    ; enabled_if : Blang.t
    ; buildable : Buildable.t
    ; rocqdep_flags : Ordered_set_lang.Unexpanded.t
    ; rocqdoc_flags : Ordered_set_lang.Unexpanded.t
    ; rocqdoc_header : String_with_vars.t option
    ; rocqdoc_footer : String_with_vars.t option
    }

  let coq_public_decode =
    let* mask = Dune_lang.Package_mask.decode () in
    map_validate
      (let+ project = Dune_project.get_exn ()
       and+ loc_name =
         field_o
           "public_name"
           (Dune_sexp.Syntax.deprecated_in
              rocq_syntax
              (0, 5)
              ~extra_info:"Please use 'package' instead."
            >>> Dune_lang.Decoder.plain_string (fun ~loc s -> loc, s))
       in
       project, loc_name)
      ~f:(fun (project, loc_name) ->
        match loc_name with
        | None -> Ok None
        | Some (loc, name) ->
          let pkg =
            match String.lsplit2 ~on:'.' name with
            | None -> Package.Name.of_string name
            | Some (pkg, _) -> Package.Name.of_string pkg
          in
          Stanza_pkg.resolve project mask (loc, pkg)
          |> Result.map ~f:(fun pkg -> Some (loc, pkg)))
  ;;

  let merge_package_public ~package ~public =
    match package, public with
    | p, None -> p
    | None, Some (_loc, pkg) -> Some pkg
    | Some _, Some (loc, _) ->
      User_error.raise
        ~loc
        [ Pp.text
            "Cannot both use 'package' and 'public_name', please remove 'public_name' as \
             it has been deprecated since version 0.5 of the Coq language. It will be \
             removed before version 1.0."
        ]
  ;;

  let boot_has_deps loc =
    User_error.raise ~loc [ Pp.textf "(boot) libraries cannot have dependencies" ]
  ;;

  let check_boot_has_no_deps boot { Buildable.theories; _ } =
    if boot
    then (
      match theories with
      | [] -> ()
      | (loc, _) :: _ -> boot_has_deps loc)
  ;;

  let check_generate_project_file (loc, generate_project_file) modules_flags =
    if generate_project_file
    then (
      match modules_flags with
      | None -> ()
      | Some _ ->
        User_error.raise
          ~loc
          [ Pp.textf "(generate_project_file) is not compatible with (modules_flags ...)"
          ])
  ;;

  module Per_file = struct
    let decode_pair =
      let+ mod_ = Rocq_module.Name.decode
      and+ flags = Ordered_set_lang.Unexpanded.decode in
      mod_, flags
    ;;

    let decode = repeat (enter decode_pair)
  end

  let decode =
    fields
      (let+ name = field "name" Rocq_lib_name.decode
       and+ package = Stanza_pkg.field_opt () >>| Option.map ~f:snd
       and+ project = Dune_project.get_exn ()
       and+ public = coq_public_decode
       and+ synopsis = field_o "synopsis" string
       and+ boot = field_b "boot" ~check:(Dune_lang.Syntax.since rocq_syntax (0, 2))
       and+ generate_project_file = located @@ field_b "generate_project_file"
       and+ modules = Ordered_set_lang.field "modules"
       and+ modules_flags =
         field_o
           "modules_flags"
           (Dune_lang.Syntax.since rocq_syntax (0, 9) >>> Per_file.decode)
       and+ enabled_if = Enabled_if.decode ~allowed_vars:Any ~since:None ()
       and+ buildable = Buildable.decode
       and+ rocqdep_flags =
         Ordered_set_lang.Unexpanded.field
           "rocqdep_flags"
           ~check:(Dune_lang.Syntax.since rocq_syntax (0, 10))
       and+ rocqdoc_flags =
         Ordered_set_lang.Unexpanded.field
           "rocqdoc_flags"
           ~check:(Dune_lang.Syntax.since rocq_syntax (0, 8))
       and+ rocqdoc_header =
         field_o
           "rocqdoc_header"
           (Dune_lang.Syntax.since rocq_syntax (0, 11) >>> String_with_vars.decode)
       and+ rocqdoc_footer =
         field_o
           "rocqdoc_footer"
           (Dune_lang.Syntax.since rocq_syntax (0, 11) >>> String_with_vars.decode)
         (* boot libraries cannot depend on other theories *)
       in
       check_boot_has_no_deps boot buildable;
       (* project files can only be generated when no per-module flags are configured. *)
       check_generate_project_file generate_project_file modules_flags;
       let package = merge_package_public ~package ~public in
       { name
       ; package
       ; project
       ; synopsis
       ; modules
       ; modules_flags
       ; boot
       ; generate_project_file
       ; buildable
       ; enabled_if
       ; rocqdep_flags
       ; rocqdoc_flags
       ; rocqdoc_header
       ; rocqdoc_footer
       })
  ;;

  include Stanza.Make (struct
      type nonrec t = t

      include Poly
    end)

  let coqlib_warn x =
    User_warning.emit
      ~loc:x.buildable.loc
      [ Pp.text
          "(coqlib ...) is deprecated and will be removed in the Coq language version \
           1.0, please use (rocq.theory ...) instead."
      ];
    x
  ;;

  let rocqlib_p = "rocqlib", decode_stanza (decode >>| coqlib_warn)
  let p = "rocq.theory", decode_stanza decode
end

let unit_stanzas =
  let+ r = return [ Theory.rocqlib_p; Theory.p; Rocqpp.p; Extraction.p ] in
  (), r
;;

let key = Dune_project.Extension.register rocq_syntax unit_stanzas Unit.to_dyn
