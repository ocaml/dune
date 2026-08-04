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
    ~name:(Syntax.Name.parse "rocq")
    ~desc:"Rocq Prover build language"
    [ (0, 11), `Since (3, 21)
    ; (0, 12), `Since (3, 22)
    ; (0, 13), `Since (3, 23)
    ; (0, 14), `Since (3, 24)
    ; (0, 15), `Since (3, 25)
    ]
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
  module Theory_dep = struct
    type t =
      | Logical of (Loc.t * Rocq_lib_name.t)
      | Package of (Loc.t * Lib_name.t)

    let loc = function
      | Logical (loc, _) | Package (loc, _) -> loc
    ;;

    let decode =
      let* rocq_lang_version = get_rocq_syntax () in
      if rocq_lang_version < (0, 15)
      then Rocq_lib_name.decode >>| fun dep -> Logical dep
      else located Lib_name.decode >>| fun dep -> Package dep
    ;;
  end

  type t =
    { flags : Ordered_set_lang.Unexpanded.t
    ; rocq_lang_version : Dune_sexp.Syntax.Version.t
    ; mode : Rocq_mode.t option
    ; use_corelib : bool
    ; plugins : (Loc.t * Lib_name.t) list (** ocaml libraries *)
    ; theories : Theory_dep.t list (** rocq libraries *)
    ; loc : Loc.t
    }

  let decode =
    let* rocq_lang_version = get_rocq_syntax () in
    let+ loc = loc
    and+ flags = Ordered_set_lang.Unexpanded.field "flags"
    and+ mode = field_o "mode" Rocq_mode.decode
    and+ stdlib_opt =
      field_o
        "stdlib"
        (Dune_lang.Syntax.deleted_in
           rocq_syntax
           (0, 14)
           ~extra_info:
             "Use (no_corelib) instead of (stdlib no) or remove the field instead of \
              writing (stdlib yes)."
         >>> enum [ "yes", true; "no", false ])
    and+ no_corelib_opt =
      field_o "no_corelib" (Dune_lang.Syntax.since rocq_syntax (0, 14) >>> return true)
    and+ plugins = field "plugins" (repeat (located Lib_name.decode)) ~default:[]
    and+ theories = field "theories" (repeat Theory_dep.decode) ~default:[] in
    let use_corelib =
      if rocq_lang_version < (0, 14)
      then Option.value stdlib_opt ~default:true
      else not (Option.value no_corelib_opt ~default:false)
    in
    { flags; mode; use_corelib; rocq_lang_version; plugins; theories; loc }
  ;;
end

module Extraction = struct
  type t =
    { target_fnames : string list
    ; prelude : Loc.t * Rocq_module.Name.t
    ; buildable : Buildable.t
    }

  let target_fnames t = t.target_fnames

  let decode =
    fields
      (let* ver = get_rocq_syntax () in
       let+ extracted_modules_opt =
         field_o
           "extracted_modules"
           (Dune_lang.Syntax.deleted_in
              rocq_syntax
              (0, 13)
              ~extra_info:
                "Use (extracted_files ...) instead, listing each .ml and .mli file \
                 explicitly."
            >>> repeat string)
       and+ extracted_files_opt =
         field_o
           "extracted_files"
           (Dune_lang.Syntax.since rocq_syntax (0, 13) >>> repeat string)
       and+ prelude = field "prelude" (located (string >>| Rocq_module.Name.make))
       and+ buildable = Buildable.decode
       and+ loc = loc in
       let target_fnames =
         if ver < (0, 13)
         then (
           match extracted_modules_opt with
           | None ->
             User_error.raise ~loc [ Pp.text "Field \"extracted_modules\" is required" ]
           | Some modules ->
             List.concat_map modules ~f:(fun m -> [ m ^ ".ml"; m ^ ".mli" ]))
         else (
           match extracted_files_opt with
           | None ->
             User_error.raise ~loc [ Pp.text "Field \"extracted_files\" is required" ]
           | Some files -> files)
       in
       { target_fnames; prelude; buildable })
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
    ; public_name : Public_lib.t option
    ; package : Package.t option
    ; project : Dune_project.t
    ; synopsis : string option
    ; modules : Ordered_set_lang.t
    ; modules_flags : (Rocq_module.Name.t * Ordered_set_lang.Unexpanded.t) list option
    ; boot : bool
    ; legacy_install : bool
    ; generate_project_file : Loc.t * bool
    ; enabled_if : Blang.t
    ; buildable : Buildable.t
    ; rocqdep_flags : Ordered_set_lang.Unexpanded.t
    ; rocqdoc_flags : Ordered_set_lang.Unexpanded.t
    ; rocqdoc_header : String_with_vars.t option
    ; rocqdoc_footer : String_with_vars.t option
    }

  let boot_has_deps loc =
    User_error.raise ~loc [ Pp.textf "(boot) libraries cannot have dependencies" ]
  ;;

  let check_boot_has_no_deps boot { Buildable.theories; _ } =
    if boot
    then (
      match theories with
      | [] -> ()
      | dep :: _ -> boot_has_deps (Buildable.Theory_dep.loc dep))
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
       and+ public_name =
         field_o
           "public_name"
           (Dune_lang.Syntax.since rocq_syntax (0, 15)
            >>> Public_lib.decode ~allow_deprecated_names:false)
       and+ package = Stanza_pkg.field_opt () >>| Option.map ~f:snd
       and+ project = Dune_project.get_exn ()
       and+ synopsis = field_o "synopsis" string
       and+ boot = field_b "boot" ~check:(Dune_lang.Syntax.since rocq_syntax (0, 2))
       and+ legacy_install =
         located
         @@ field_b "legacy_install" ~check:(Dune_lang.Syntax.since rocq_syntax (0, 15))
       and+ generate_project_file = located @@ field_b "generate_project_file"
       and+ modules = Ordered_set_lang.field "modules"
       and+ modules_flags = field_o "modules_flags" Per_file.decode
       and+ enabled_if = Enabled_if.decode ~allowed_vars:Any ~since:None ()
       and+ buildable = Buildable.decode
       and+ rocqdep_flags = Ordered_set_lang.Unexpanded.field "rocqdep_flags"
       and+ rocqdoc_flags = Ordered_set_lang.Unexpanded.field "rocqdoc_flags"
       and+ rocqdoc_header = field_o "rocqdoc_header" String_with_vars.decode
       and+ rocqdoc_footer = field_o "rocqdoc_footer" String_with_vars.decode in
       let package =
         match public_name with
         | None -> package
         | Some public_name -> Some (Public_lib.package public_name)
       in
       let legacy_install_loc, legacy_install = legacy_install in
       if legacy_install && Option.is_none public_name
       then
         User_error.raise
           ~loc:legacy_install_loc
           [ Pp.text "legacy_install requires public_name" ];
       (* boot libraries cannot depend on other theories *)
       check_boot_has_no_deps boot buildable;
       (* project files can only be generated when no per-module flags are configured. *)
       check_generate_project_file generate_project_file modules_flags;
       { name
       ; public_name
       ; package
       ; project
       ; synopsis
       ; modules
       ; modules_flags
       ; boot
       ; legacy_install
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

  let p = "rocq.theory", decode_stanza decode
end

let unit_stanzas =
  let+ r = return [ Theory.p; Rocqpp.p; Extraction.p ] in
  (), r
;;

let key = Dune_project.Extension.register rocq_syntax unit_stanzas Unit.to_dyn
