open Import
module Rule = Dune_engine.Rule

module Promote = struct
  module Into = struct
    type t =
      { loc : Loc.t
      ; dir : String_with_vars.t
      }
  end

  type t =
    { lifetime : Rule.Promote.Lifetime.t
    ; into : Into.t option
    ; only : Filename.t Predicate.t option
    }
end

type t =
  | Standard
  | Fallback (** Only use this rule if the source files don't exist. *)
  | Promote of Promote.t (** Silently promote the targets to the source tree. *)
  | Ignore_source_files
  (** Just ignore the source files entirely. This is for cases where the
      targets are promoted only in a specific context, such as for
      .install files. *)
