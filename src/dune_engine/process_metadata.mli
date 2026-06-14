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

val default : t

val create
  :  ?loc:Loc.t
  -> ?compound:User_message.Compound.t list
  -> ?has_embedded_location:bool
  -> ?name:string
  -> ?categories:string list
  -> ?can_run_in_action_runner:bool
  -> ?purpose:purpose
  -> ?promotion:User_message.Diff_annot.t
  -> unit
  -> t
