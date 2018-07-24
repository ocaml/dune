open Stdune
open Sexp.Of_sexp

let field_oslu name = Ordered_set_lang.Unexpanded.field name

module Flags = struct
  type 'flag t =
    { compile : 'flag
    ; link    : 'flag
    }

  let fields =
    let%map compile = field_oslu "flags"
    and link = field_oslu "link_flags" (* TODO (1, 1) only *)
    in
    { compile
    ; link
    }

  let empty =
    { compile = []
    ; link = []
    }

  let default ~profile =
    if profile = "dev" then
      { compile = ["--pretty"; "--source-map-inline"]
      ; link    = ["--source-map-inline"]
      }
    else
      empty

  let map { compile; link } ~f =
    { compile = f compile
    ; link    = f link
    }

  let default_user_written =
    { compile = Ordered_set_lang.Unexpanded.standard
    ; link = Ordered_set_lang.Unexpanded.standard
    }

  type user_written = Ordered_set_lang.Unexpanded.t t
end

module In_buildable = struct
  type t =
    { flags            : Flags.user_written
    ; javascript_files : string list
    }

  let t =
    record
      (let%map flags = Flags.fields
       and javascript_files = field "javascript_files" (list string) ~default:[]
       in
       { flags
       ; javascript_files
       })

  let flags t = t.flags

  let default =
    { flags = Flags.default_user_written
    ; javascript_files = []
    }

  let field = field "js_of_ocaml" t ~default
end

module Compilation = struct
  type t =
    | Separate
    | Classic

  let default ~profile =
    if profile = "dev" then
      Separate
    else
      Classic

  let t =
    Syntax.since Stanza.syntax (1, 1) >>= fun () ->
    enum
      [ "separate", Separate
      ; "classic", Classic
      ]
end

module Env = struct
  type t =
    { flags       : Flags.user_written
    ; compilation : Compilation.t option
    }

  let default =
    { flags = Flags.default_user_written
    ; compilation = None
    }

  let t =
    record
      (let%map flags = Flags.fields
       and compilation = field_o "compilation" Compilation.t
       in
       { flags
       ; compilation
       })

  let field = field "jsoo" t ~default
end
