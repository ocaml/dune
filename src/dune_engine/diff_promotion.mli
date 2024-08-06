(** This module is responsible for handling [diff]-related file promotions.

    See [Target_promotion] for the logic that handles promotion of rule targets. *)

open Import

module Annot : sig
  type t =
    { in_source : Path.Source.t
    ; in_build : Path.Build.t
    }

  val annot : t User_message.Annots.Key.t
end
