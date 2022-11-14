open Import
open Dune_lang.Decoder

let coq_syntax =
  Dune_lang.Syntax.create ~name:"coq" ~desc:"the Coq language"
    [ ((0, 1), `Since (1, 9))
    ; ((0, 2), `Since (2, 5))
    ; ((0, 3), `Since (2, 8))
    ; ((0, 4), `Since (3, 3))
    ; ((0, 5), `Since (3, 4))
    ; ((0, 6), `Since (3, 5))
    ]

module Coqpp = struct
  type t =
    { modules : Ordered_set_lang.t
    ; loc : Loc.t
    }

  let decode =
    fields
      (let+ modules = Ordered_set_lang.field "modules"
       and+ loc = loc in
       { modules; loc })

  type Stanza.t += T of t

  let p = ("coq.pp", decode >>| fun x -> [ T x ])
end

module Buildable = struct
  type t =
    { flags : Ordered_set_lang.Unexpanded.t
    ; coq_lang_version : Dune_sexp.Syntax.Version.t
    ; mode : Loc.t * Coq_mode.t
    ; use_stdlib : bool
    ; plugins : (Loc.t * Lib_name.t) list  (** ocaml libraries *)
    ; theories : (Loc.t * Coq_lib_name.t) list  (** coq libraries *)
    ; loc : Loc.t
    }

  let merge_plugins_libraries ~plugins ~libraries =
    match (plugins, libraries) with
    | p, [] -> p
    | [], ls -> ls
    | _, (loc, _) :: _ ->
      User_error.raise ~loc
        [ Pp.text
            "Cannot both use 'plugins' and 'libraries', please remove \
             'libraries' as it has been deprecated since version 0.5 of the \
             Coq language. It will be removed before version 1.0."
        ]

  let decode =
    let* coq_lang_version = Dune_lang.Syntax.get_exn coq_syntax in
    let+ loc = loc
    and+ flags = Ordered_set_lang.Unexpanded.field "flags"
    and+ mode =
      let default =
        if coq_lang_version < (0, 3) then Coq_mode.Legacy else Coq_mode.VoOnly
      in
      located
        (field "mode" ~default
           (Dune_lang.Syntax.since coq_syntax (0, 3) >>> Coq_mode.decode))
    and+ use_stdlib =
      field ~default:true "stdlib"
        (Dune_lang.Syntax.since coq_syntax (0, 6)
        >>> enum [ ("yes", true); ("no", false) ])
    and+ libraries =
      field "libraries" ~default:[]
        (Dune_sexp.Syntax.deprecated_in coq_syntax (0, 5)
           ~extra_info:"It has been renamed to 'plugins'."
        >>> repeat (located Lib_name.decode))
    and+ plugins =
      field "plugins" (repeat (located Lib_name.decode)) ~default:[]
    and+ theories =
      field "theories"
        (Dune_lang.Syntax.since coq_syntax (0, 2) >>> repeat Coq_lib_name.decode)
        ~default:[]
    in
    let plugins = merge_plugins_libraries ~plugins ~libraries in
    { flags; mode; use_stdlib; coq_lang_version; plugins; theories; loc }
end

module Extraction = struct
  type t =
    { (* not a list of modules because we want to preserve whatever case coq
         uses *)
      extracted_modules : string list
    ; prelude : Loc.t * Coq_module.Name.t
    ; buildable : Buildable.t
    }

  let ml_target_fnames t =
    List.concat_map t.extracted_modules ~f:(fun m -> [ m ^ ".ml"; m ^ ".mli" ])

  let decode =
    fields
      (let+ extracted_modules = field "extracted_modules" (repeat string)
       and+ prelude =
         field "prelude" (located (string >>| Coq_module.Name.make))
       and+ buildable = Buildable.decode in
       { prelude; extracted_modules; buildable })

  type Stanza.t += T of t

  let p = ("coq.extraction", decode >>| fun x -> [ T x ])
end

module Theory = struct
  type t =
    { name : Loc.t * Coq_lib_name.t
    ; package : Package.t option
    ; project : Dune_project.t
    ; synopsis : string option
    ; modules : Ordered_set_lang.t
    ; boot : bool
    ; enabled_if : Blang.t
    ; buildable : Buildable.t
    }

  let coq_public_decode =
    map_validate
      (let+ project = Dune_project.get_exn ()
       and+ loc_name =
         field_o "public_name"
           (Dune_sexp.Syntax.deprecated_in coq_syntax (0, 5)
              ~extra_info:"Please use 'package' instead."
           >>> Dune_lang.Decoder.plain_string (fun ~loc s -> (loc, s)))
       in
       (project, loc_name))
      ~f:(fun (project, loc_name) ->
        match loc_name with
        | None -> Ok None
        | Some (loc, name) ->
          let pkg =
            match String.lsplit2 ~on:'.' name with
            | None -> Package.Name.of_string name
            | Some (pkg, _) -> Package.Name.of_string pkg
          in
          Stanza_common.Pkg.resolve project pkg
          |> Result.map ~f:(fun pkg -> Some (loc, pkg)))

  let merge_package_public ~package ~public =
    match (package, public) with
    | p, None -> p
    | None, Some (_loc, pkg) -> Some pkg
    | Some _, Some (loc, _) ->
      User_error.raise ~loc
        [ Pp.text
            "Cannot both use 'package' and 'public_name', please remove \
             'public_name' as it has been deprecated since version 0.5 of the \
             Coq language. It will be removed before version 1.0."
        ]

  let boot_has_deps loc =
    User_error.raise ~loc
      [ Pp.textf "(boot) libraries cannot have dependencies" ]

  let check_boot_has_no_deps boot { Buildable.theories; _ } =
    if boot then
      match theories with
      | [] -> ()
      | (loc, _) :: _ -> boot_has_deps loc

  let decode =
    fields
      (let+ name = field "name" Coq_lib_name.decode
       and+ package = field_o "package" Stanza_common.Pkg.decode
       and+ project = Dune_project.get_exn ()
       and+ public = coq_public_decode
       and+ synopsis = field_o "synopsis" string
       and+ boot =
         field_b "boot" ~check:(Dune_lang.Syntax.since coq_syntax (0, 2))
       and+ modules = Stanza_common.modules_field "modules"
       and+ enabled_if = Enabled_if.decode ~allowed_vars:Any ~since:None ()
       and+ buildable = Buildable.decode in
       (* boot libraries cannot depend on other theories *)
       check_boot_has_no_deps boot buildable;
       let package = merge_package_public ~package ~public in
       { name
       ; package
       ; project
       ; synopsis
       ; modules
       ; boot
       ; buildable
       ; enabled_if
       })

  type Stanza.t += T of t

  let coqlib_warn x =
    User_warning.emit ~loc:x.buildable.loc
      [ Pp.text
          "(coqlib ...) is deprecated and will be removed in the Coq language \
           version 1.0, please use (coq.theory ...) instead."
      ];
    x

  let coqlib_p = ("coqlib", decode >>| fun x -> [ T (coqlib_warn x) ])

  let p = ("coq.theory", decode >>| fun x -> [ T x ])
end

let unit_stanzas =
  let+ r = return [ Theory.coqlib_p; Theory.p; Coqpp.p; Extraction.p ] in
  ((), r)

let key = Dune_project.Extension.register coq_syntax unit_stanzas Unit.to_dyn
