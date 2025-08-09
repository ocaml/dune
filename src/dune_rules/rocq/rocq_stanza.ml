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
    [ (0, 11), `Since (3, 20) ]
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

  let p = "rocq.pp", decode >>| fun x -> [ make_stanza x ]
end

module Buildable = struct
  type t =
    { flags : Ordered_set_lang.Unexpanded.t
    ; rocq_lang_version : Dune_sexp.Syntax.Version.t
    ; mode : Rocq_mode.t option
    ; use_stdlib : bool
    ; plugins : (Loc.t * Lib_name.t) list (** ocaml libraries *)
    ; theories : (Loc.t * Rocq_lib_name.t) list (** rocq libraries *)
    ; loc : Loc.t
    }

  let decode =
    let* rocq_lang_version = get_rocq_syntax () in
    let+ loc = loc
    and+ flags = Ordered_set_lang.Unexpanded.field "flags"
    and+ mode = field_o "mode" Rocq_mode.decode
    and+ use_stdlib = field ~default:true "stdlib" (enum [ "yes", true; "no", false ])
    and+ plugins = field "plugins" (repeat (located Lib_name.decode)) ~default:[]
    and+ theories = field "theories" (repeat Rocq_lib_name.decode) ~default:[] in
    { flags; mode; use_stdlib; rocq_lang_version; plugins; theories; loc }
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

  let p = "rocq.extraction", decode >>| fun x -> [ make_stanza x ]
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
    ; enabled_if : Blang.t
    ; buildable : Buildable.t
    ; rocqdep_flags : Ordered_set_lang.Unexpanded.t
    ; rocqdoc_flags : Ordered_set_lang.Unexpanded.t
    }

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

  module Per_file = struct
    let decode_pair =
      let+ mod_ = Rocq_module.Name.decode
      and+ flags = Ordered_set_lang.Unexpanded.decode in
      mod_, flags
    ;;

    let decode = enter (repeat decode_pair)
  end

  let decode =
    fields
      (let+ name = field "name" Rocq_lib_name.decode
       and+ package = field_o "package" Stanza_common.Pkg.decode
       and+ project = Dune_project.get_exn ()
       and+ synopsis = field_o "synopsis" string
       and+ boot = field_b "boot" ~check:(Dune_lang.Syntax.since rocq_syntax (0, 2))
       and+ modules = Ordered_set_lang.field "modules"
       and+ modules_flags = field_o "modules_flags" Per_file.decode
       and+ enabled_if = Enabled_if.decode ~allowed_vars:Any ~since:None ()
       and+ buildable = Buildable.decode
       and+ rocqdep_flags = Ordered_set_lang.Unexpanded.field "rocqdep_flags"
       and+ rocqdoc_flags = Ordered_set_lang.Unexpanded.field "rocqdoc_flags" in
       (* boot libraries cannot depend on other theories *)
       check_boot_has_no_deps boot buildable;
       { name
       ; package
       ; project
       ; synopsis
       ; modules
       ; modules_flags
       ; boot
       ; buildable
       ; enabled_if
       ; rocqdep_flags
       ; rocqdoc_flags
       })
  ;;

  include Stanza.Make (struct
      type nonrec t = t

      include Poly
    end)

  let p = "rocq.theory", decode >>| fun x -> [ make_stanza x ]
end

let unit_stanzas =
  let+ r = return [ Theory.p; Rocqpp.p; Extraction.p ] in
  (), r
;;

let key = Dune_project.Extension.register rocq_syntax unit_stanzas Unit.to_dyn
