module Ppx_args : sig
  module Cookie : sig
    type t =
      { name : string
      ; value : String_with_vars.t
      }
  end

  type t =
    { cookies : Cookie.t list
    }
end

type t =
  | Normal
  | Ppx_deriver of Ppx_args.t
  | Ppx_rewriter of Ppx_args.t

include Dune_lang.Conv with type t := t
