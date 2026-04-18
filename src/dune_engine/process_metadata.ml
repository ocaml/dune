open Import

type purpose =
  | Internal_job
  | Build_job of Targets.Validated.t option

type t =
  { loc : Loc.t option
  ; compound : User_message.Compound.t list
  ; name : string option
  ; categories : string list
  ; can_run_in_action_runner : bool
  ; purpose : purpose
  ; has_embedded_location : bool
  ; promotion : User_message.Diff_annot.t option
  }

let default =
  { loc = None
  ; compound = []
  ; purpose = Internal_job
  ; categories = []
  ; name = None
  ; can_run_in_action_runner = false
  ; has_embedded_location = false
  ; promotion = None
  }
;;

let create
      ?loc
      ?(compound = default.compound)
      ?(has_embedded_location = false)
      ?name
      ?(categories = default.categories)
      ?(can_run_in_action_runner = false)
      ?(purpose = Internal_job)
      ?promotion
      ()
  =
  { loc
  ; compound
  ; name
  ; categories
  ; can_run_in_action_runner
  ; purpose
  ; has_embedded_location
  ; promotion
  }
;;
