open Import

(** [Button.of_ ui f] turns the given [ui] into a clickable button that calls [f] when
    clicked. *)
val of_ : Ui.t -> (unit -> unit) -> Ui.t
