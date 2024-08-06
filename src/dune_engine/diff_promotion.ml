open Import

module Annot = struct
  type t =
    { in_source : Path.Source.t
    ; in_build : Path.Build.t
    }

  let to_dyn { in_source; in_build } =
    let open Dyn in
    record
      [ "in_source", Path.Source.to_dyn in_source
      ; "in_build", Path.Build.to_dyn in_build
      ]
  ;;

  let annot = User_message.Annots.Key.create ~name:"promote" to_dyn
end
