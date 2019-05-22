module Style = struct
  type t =
    | Loc
    | Error
    | Warning
    | Kwd
    | Id
    | Prompt
    | Details
    | Ok
    | Debug
end

type t =
  { loc : Loc.t option
  ; paragraphs : Style.t Pp.t list
  }

exception E of t

let raise ?loc paragraphs =
  raise (E { loc; paragraphs })

let pp { loc = _; paragraphs } =
  let error = Pp.text "Error:" in
  let paragraphs =
    match paragraphs with
    | [] -> [ error ]
    | x :: l -> Pp.box [error; Pp.space; x ] :: l
  in
  Pp.vbox [ Pp.concat paragraphs ~sep:Pp.cut ]

let () =
  Printexc.register_printer (function
    | E t -> Some (Format.asprintf "%a@?" Pp.pp (pp t |> Pp.map_tags ~f:ignore))
    | _ -> None)

module Ansi_output = struct
  type config = (Style.t -> Ansi_color.Style.t list)

  let default_config : config = function
    | Loc     -> [Bold]
    | Error   -> [Bold; Fg Red]
    | Warning -> [Bold; Fg Magenta]
    | Kwd     -> [Bold; Fg Blue]
    | Id      -> [Bold; Fg Yellow]
    | Prompt  -> [Bold; Fg Green]
    | Details -> [Dim; Fg White]
    | Ok      -> [Dim; Fg Green]
    | Debug   -> [Underlined; Fg Bright_cyan]

  module Tag_handler = struct
    type t =
      { config : config
      ; ansi_handler : Ansi_color.Render.Tag.Handler.t
      }

    let init =
      { config = default_config
      ; ansi_handler = Ansi_color.Render.Tag.Handler.init
      }

    let handle { config; ansi_handler } style =
      let before, ansi_handler, after =
        Ansi_color.Render.Tag.Handler.handle ansi_handler (config style)
      in
      before, { config; ansi_handler }, after
  end

  module Render = Pp.Renderer.Make(struct
      type t = Style.t
      module Handler = Tag_handler
    end)

  let print_without_colors =

  let create oc =
    let renderer = lazy (Staged.unstage (Render.channel oc)) in
    fun ?(config=default_config) ?margin pp ->
      let tag_handler : Render.Tag.Handler.t =
        { Render.Tag.Handler.init with config }
      in
      Lazy.force renderer ~tag_handler ?margin pp)

  let print = create stdout
  let prerr = create stderr
end
