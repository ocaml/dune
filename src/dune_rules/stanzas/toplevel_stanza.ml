open Import

type t =
  { name : string
  ; libraries : (Loc.t * Lib_name.t) list
  ; loc : Loc.t
  ; pps : Preprocess.Without_instrumentation.t Preprocess.t
  }

include Stanza.Make (struct
    type nonrec t = t

    include Poly
  end)

let decode =
  let open Dune_lang.Decoder in
  fields
    (let+ loc = loc
     and+ name = field "name" string
     and+ libraries = field "libraries" (repeat (located Lib_name.decode)) ~default:[]
     and+ pps =
       field
         "preprocess"
         (Dune_lang.Syntax.since Stanza.syntax (2, 5) >>> Preprocess.decode)
         ~default:Preprocess.No_preprocessing
     in
     match pps with
     | Preprocess.Pps _ | No_preprocessing -> { name; libraries; loc; pps }
     | Action (loc, _) | Future_syntax loc ->
       User_error.raise
         ~loc
         [ Pp.text
             "Toplevel does not currently support action or future_syntax preprocessing."
         ])
;;
