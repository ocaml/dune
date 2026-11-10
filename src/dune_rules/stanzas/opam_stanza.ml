open Import
open Dune_lang.Decoder

type t =
  { loc : Loc.t
  ; package : Package.t
  ; dir : Filename.t
  ; version : Package_version.t
  ; dev : bool
  ; source : String_with_vars.t option
  ; files : (String_with_vars.t * Path.Local.t) list
  ; recipe : Opam_build.Recipe.t
  }

include Stanza.Make (struct
    type nonrec t = t

    include Poly
  end)

let decode =
  let* () = Dune_lang.Unreleased.since () in
  fields
  @@ let* loc = loc
     and+ package = Stanza_pkg.field ~stanza:"opam"
     and+ dir = Source.Dune_file.opam_dir
     and+ version = field "version" Package_version.decode
     and+ dev = field_b "dev"
     and+ source = field_o "source" String_with_vars.decode
     and+ files =
       field
         "files"
         ~default:[]
         (repeat
            (pair String_with_vars.decode (plain_string Path.Local.parse_string_exn)))
     in
     let+ recipe =
       let env = Pform.Env.pkg Dune_lang.Pkg.syntax (0, 1) in
       String_with_vars.set_decoding_env
         env
         (let+ build = field_o "build" Dune_lang.Action.decode_pkg
          and+ install = field_o "install" Dune_lang.Action.decode_pkg
          and+ depexts = field "depexts" ~default:[] (repeat string)
          and+ exported_env =
            field "exported_env" ~default:[] (repeat Dune_lang.Action.Env_update.decode)
          in
          { Opam_build.Recipe.build
          ; install
          ; exported_env
          ; depexts =
              [ { Dune_pkg.Lock_dir.Depexts.external_package_names = depexts
                ; enabled_if = `Always
                }
              ]
          })
     in
     { loc; package; dir; version; dev; source; files; recipe }
;;
