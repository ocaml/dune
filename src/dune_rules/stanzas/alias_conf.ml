open Import

type t =
  { name : Alias.Name.t
  ; deps : Dep_conf.t Bindings.t
  ; action : (Loc.t * Dune_lang.Action.t) option
  ; locks : Locks.t
  ; package : Package.t option
  ; enabled_if : Blang.t
  ; loc : Loc.t
  }

include Stanza.Make (struct
    type nonrec t = t

    include Poly
  end)

let decode =
  let open Dune_lang.Decoder in
  fields
    (let* deps = field "deps" (Bindings.decode Dep_conf.decode) ~default:Bindings.empty in
     String_with_vars.add_user_vars_to_decoding_env
       (Bindings.var_names deps)
       (let+ name = field "name" Dune_lang.Alias.decode
        and+ package = field_o "package" Stanza_common.Pkg.decode
        and+ action =
          field_o
            "action"
            (Dune_lang.Syntax.deleted_in
               Stanza.syntax
               (2, 0)
               ~extra_info:"Use a rule stanza with the alias field instead"
             >>> located Dune_lang.Action.decode_dune_file)
        and+ loc = loc
        and+ locks = Locks.field ()
        and+ enabled_if = field "enabled_if" Blang.decode ~default:Blang.true_ in
        { name; deps; action; package; locks; enabled_if; loc }))
;;
