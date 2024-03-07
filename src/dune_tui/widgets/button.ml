open Import

let of_ ui f =
  Ui.mouse_area
    (fun ~x:_ ~y:_ _ ->
      f ();
      `Handled)
    ui
;;
