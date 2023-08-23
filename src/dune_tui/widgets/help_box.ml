open Import
open Lwd.O

let hsnap_or_leave ~width img =
  if I.width img < width then I.hsnap ~align:`Middle width img else img
;;

let vsnap_or_leave ~height img =
  if I.height img < height then I.vsnap ~align:`Middle height img else img
;;

let dialogue_box ~attr ~title ~title_attr ~width ~height image =
  Drawing.box_with_title ~attr ~title ~title_attr image
  |> vsnap_or_leave ~height
  |> hsnap_or_leave ~width
;;

type t =
  { ui : Ui.t
  ; toggle : unit -> unit
  }

let make =
  let help_screen_enabled = Lwd.var false in
  let handle_help () = Lwd.set help_screen_enabled (not (Lwd.peek help_screen_enabled)) in
  fun ~helper_attr ~divider_attr ~help_screen_lines ~width ~height ->
    let+ ui =
      Lwd.get help_screen_enabled
      >>| function
      | false -> Ui.empty
      | true ->
        let image =
          let img = List.map help_screen_lines ~f:(I.string helper_attr) |> I.vcat in
          [ img
          ; I.string A.empty ""
          ; I.string helper_attr "🐪 Developed by the Dune team 🐪"
            |> I.hsnap ~align:`Middle (I.width img)
          ]
          |> I.vcat
          |> I.pad ~l:1 ~r:1 ~t:1 ~b:1
          |> dialogue_box
               ~attr:divider_attr
               ~title:"Help"
               ~title_attr:helper_attr
               ~width
               ~height
        in
        (* Any mouse actions should close the help screen. *)
        let mouse_handler ~x:_ ~y:_ = function
          | `Left | `Middle | `Right | `Scroll _ ->
            handle_help ();
            `Handled
        in
        (* Any keyboard actions should close the help screen. *)
        let keyboard_handler : Ui.key -> Ui.may_handle = function
          | _ ->
            handle_help ();
            `Handled
        in
        Ui.atom image |> Ui.mouse_area mouse_handler |> Ui.keyboard_area keyboard_handler
    in
    { ui; toggle = handle_help }
;;
