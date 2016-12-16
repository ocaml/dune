open! Import

type t = Byte | Native

let all = [Byte; Native]

let t =
  let open Sexp.Of_sexp in
  sum
    [ cstr "byte"   [] Byte
    ; cstr "native" [] Native
    ]

let choose byte native = function
  | Byte   -> byte
  | Native -> native

let compiled_unit_ext = choose ".cmo" ".cmx"
let compiled_lib_ext = choose ".cma" ".cmxa"

let compiler t (ctx : Context.t) = choose (Some ctx.ocamlc) ctx.ocamlopt t

let findlib_predicate = choose "byte" "native"

let cm_kind = choose Cm_kind.Cmo Cmx

let exe_ext = choose ".bc" ".exe"

let best (ctx : Context.t) =
  match ctx.ocamlopt with
  | Some _ -> Native
  | None   -> Byte

let of_cm_kind : Cm_kind.t -> t = function
  | Cmi | Cmo -> Byte
  | Cmx -> Native

module Dict = struct
  type 'a t =
    { byte   : 'a
    ; native : 'a
    }

  let get t = function
    | Byte   -> t.byte
    | Native -> t.native

  let of_func f =
    { byte   = f ~mode:Byte
    ; native = f ~mode:Native
    }

  let map2 a b ~f =
    { byte   = f a.byte   b.byte
    ; native = f a.native b.native
    }
end
