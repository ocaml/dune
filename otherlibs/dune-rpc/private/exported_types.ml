open Import

module Loc = struct
  type t = Stdune.Lexbuf.Loc.t =
    { start : Lexing.position
    ; stop : Lexing.position
    }

  let start t = t.start
  let stop t = t.stop

  let pos_sexp =
    let open Conv in
    let to_ (pos_fname, pos_lnum, pos_bol, pos_cnum) =
      { Lexing.pos_fname; pos_lnum; pos_bol; pos_cnum }
    in
    let from { Lexing.pos_fname; pos_lnum; pos_bol; pos_cnum } =
      pos_fname, pos_lnum, pos_bol, pos_cnum
    in
    let pos_fname = field "pos_fname" (required string) in
    let pos_lnum = field "pos_lnum" (required int) in
    let pos_bol = field "pos_bol" (required int) in
    let pos_cnum = field "pos_cnum" (required int) in
    iso (record (four pos_fname pos_lnum pos_bol pos_cnum)) to_ from
  ;;

  let sexp =
    let open Conv in
    let to_ (start, stop) = { start; stop } in
    let from { start; stop } = start, stop in
    let start = field "start" (required pos_sexp) in
    let stop = field "stop" (required pos_sexp) in
    iso (record (both start stop)) to_ from
  ;;
end

module Ansi_color = struct
  module RGB8 = struct
    include Stdune.Ansi_color.RGB8

    let sexp =
      Conv.iso Conv.char Stdune.Ansi_color.RGB8.of_char Stdune.Ansi_color.RGB8.to_char
    ;;
  end

  module RGB24 = struct
    include Stdune.Ansi_color.RGB24

    let sexp =
      Conv.iso Conv.int Stdune.Ansi_color.RGB24.of_int Stdune.Ansi_color.RGB24.to_int
    ;;
  end

  module Style = struct
    type t = Stdune.Ansi_color.Style.t

    let sexp =
      let open Conv in
      let fg_default = constr "Fg_default" unit (fun () -> `Fg_default) in
      let fg_black = constr "Fg_black" unit (fun () -> `Fg_black) in
      let fg_red = constr "Fg_red" unit (fun () -> `Fg_red) in
      let fg_green = constr "Fg_green" unit (fun () -> `Fg_green) in
      let fg_yellow = constr "Fg_yellow" unit (fun () -> `Fg_yellow) in
      let fg_blue = constr "Fg_blue" unit (fun () -> `Fg_blue) in
      let fg_magenta = constr "Fg_magenta" unit (fun () -> `Fg_magenta) in
      let fg_cyan = constr "Fg_cyan" unit (fun () -> `Fg_cyan) in
      let fg_white = constr "Fg_white" unit (fun () -> `Fg_white) in
      let fg_bright_black = constr "Fg_bright_black" unit (fun () -> `Fg_bright_black) in
      let fg_bright_red = constr "Fg_bright_red" unit (fun () -> `Fg_bright_red) in
      let fg_bright_green = constr "Fg_bright_green" unit (fun () -> `Fg_bright_green) in
      let fg_bright_yellow =
        constr "Fg_bright_yellow" unit (fun () -> `Fg_bright_yellow)
      in
      let fg_bright_blue = constr "Fg_bright_blue" unit (fun () -> `Fg_bright_blue) in
      let fg_bright_magenta =
        constr "Fg_bright_magenta" unit (fun () -> `Fg_bright_magenta)
      in
      let fg_bright_cyan = constr "Fg_bright_cyan" unit (fun () -> `Fg_bright_cyan) in
      let fg_bright_white = constr "Fg_bright_white" unit (fun () -> `Fg_bright_white) in
      let fg_8_bit_color =
        constr "Fg_8_bit_color" RGB8.sexp (fun c -> `Fg_8_bit_color c)
      in
      let fg_24_bit_color =
        constr "Fg_24_bit_color" RGB24.sexp (fun c -> `Fg_24_bit_color c)
      in
      let bg_default = constr "Bg_default" unit (fun () -> `Bg_default) in
      let bg_black = constr "Bg_black" unit (fun () -> `Bg_black) in
      let bg_red = constr "Bg_red" unit (fun () -> `Bg_red) in
      let bg_green = constr "Bg_green" unit (fun () -> `Bg_green) in
      let bg_yellow = constr "Bg_yellow" unit (fun () -> `Bg_yellow) in
      let bg_blue = constr "Bg_blue" unit (fun () -> `Bg_blue) in
      let bg_magenta = constr "Bg_magenta" unit (fun () -> `Bg_magenta) in
      let bg_cyan = constr "Bg_cyan" unit (fun () -> `Bg_cyan) in
      let bg_white = constr "Bg_white" unit (fun () -> `Bg_white) in
      let bg_bright_black = constr "Bg_bright_black" unit (fun () -> `Bg_bright_black) in
      let bg_bright_red = constr "Bg_bright_red" unit (fun () -> `Bg_bright_red) in
      let bg_bright_green = constr "Bg_bright_green" unit (fun () -> `Bg_bright_green) in
      let bg_bright_yellow =
        constr "Bg_bright_yellow" unit (fun () -> `Bg_bright_yellow)
      in
      let bg_bright_blue = constr "Bg_bright_blue" unit (fun () -> `Bg_bright_blue) in
      let bg_bright_magenta =
        constr "Bg_bright_magenta" unit (fun () -> `Bg_bright_magenta)
      in
      let bg_bright_cyan = constr "Bg_bright_cyan" unit (fun () -> `Bg_bright_cyan) in
      let bg_bright_white = constr "Bg_bright_white" unit (fun () -> `Bg_bright_white) in
      let bg_8_bit_color =
        constr "Bg_8_bit_color" RGB8.sexp (fun c -> `Bg_8_bit_color c)
      in
      let bg_24_bit_color =
        constr "Bg_24_bit_color" RGB24.sexp (fun c -> `Bg_24_bit_color c)
      in
      let bold = constr "Bold" unit (fun () -> `Bold) in
      let dim = constr "Dim" unit (fun () -> `Dim) in
      let italic = constr "Italic" unit (fun () -> `Italic) in
      let underline = constr "Underline" unit (fun () -> `Underline) in
      sum
        [ econstr fg_default
        ; econstr fg_black
        ; econstr fg_red
        ; econstr fg_green
        ; econstr fg_yellow
        ; econstr fg_blue
        ; econstr fg_magenta
        ; econstr fg_cyan
        ; econstr fg_white
        ; econstr fg_bright_black
        ; econstr fg_bright_red
        ; econstr fg_bright_green
        ; econstr fg_bright_yellow
        ; econstr fg_bright_blue
        ; econstr fg_bright_magenta
        ; econstr fg_bright_cyan
        ; econstr fg_bright_white
        ; econstr fg_8_bit_color
        ; econstr fg_24_bit_color
        ; econstr bg_default
        ; econstr bg_black
        ; econstr bg_red
        ; econstr bg_green
        ; econstr bg_yellow
        ; econstr bg_blue
        ; econstr bg_magenta
        ; econstr bg_cyan
        ; econstr bg_white
        ; econstr bg_bright_black
        ; econstr bg_bright_red
        ; econstr bg_bright_green
        ; econstr bg_bright_yellow
        ; econstr bg_bright_blue
        ; econstr bg_bright_magenta
        ; econstr bg_bright_cyan
        ; econstr bg_bright_white
        ; econstr bg_8_bit_color
        ; econstr bg_24_bit_color
        ; econstr bold
        ; econstr dim
        ; econstr italic
        ; econstr underline
        ]
        (function
          | `Fg_default -> case () fg_default
          | `Fg_black -> case () fg_black
          | `Fg_red -> case () fg_red
          | `Fg_green -> case () fg_green
          | `Fg_yellow -> case () fg_yellow
          | `Fg_blue -> case () fg_blue
          | `Fg_magenta -> case () fg_magenta
          | `Fg_cyan -> case () fg_cyan
          | `Fg_white -> case () fg_white
          | `Fg_bright_black -> case () fg_bright_black
          | `Fg_bright_red -> case () fg_bright_red
          | `Fg_bright_green -> case () fg_bright_green
          | `Fg_bright_yellow -> case () fg_bright_yellow
          | `Fg_bright_blue -> case () fg_bright_blue
          | `Fg_bright_magenta -> case () fg_bright_magenta
          | `Fg_bright_cyan -> case () fg_bright_cyan
          | `Fg_bright_white -> case () fg_bright_white
          | `Fg_8_bit_color c -> case c fg_8_bit_color
          | `Fg_24_bit_color c -> case c fg_24_bit_color
          | `Bg_default -> case () bg_default
          | `Bg_black -> case () bg_black
          | `Bg_red -> case () bg_red
          | `Bg_green -> case () bg_green
          | `Bg_yellow -> case () bg_yellow
          | `Bg_blue -> case () bg_blue
          | `Bg_magenta -> case () bg_magenta
          | `Bg_cyan -> case () bg_cyan
          | `Bg_white -> case () bg_white
          | `Bg_bright_black -> case () bg_bright_black
          | `Bg_bright_red -> case () bg_bright_red
          | `Bg_bright_green -> case () bg_bright_green
          | `Bg_bright_yellow -> case () bg_bright_yellow
          | `Bg_bright_blue -> case () bg_bright_blue
          | `Bg_bright_magenta -> case () bg_bright_magenta
          | `Bg_bright_cyan -> case () bg_bright_cyan
          | `Bg_bright_white -> case () bg_bright_white
          | `Bg_8_bit_color c -> case c bg_8_bit_color
          | `Bg_24_bit_color c -> case c bg_24_bit_color
          | `Bold -> case () bold
          | `Dim -> case () dim
          | `Italic -> case () italic
          | `Underline -> case () underline)
    ;;
  end
end

module User_message = struct
  module Style = struct
    type t = Stdune.User_message.Style.t =
      | Loc
      | Error
      | Warning
      | Kwd
      | Id
      | Prompt
      | Hint
      | Details
      | Ok
      | Debug
      | Success
      | Ansi_styles of Ansi_color.Style.t list

    let sexp =
      let open Conv in
      let loc = constr "Loc" unit (fun () -> Loc) in
      let error = constr "Error" unit (fun () -> Error) in
      let warning = constr "Warning" unit (fun () -> Warning) in
      let kwd = constr "Kwd" unit (fun () -> Kwd) in
      let id = constr "Id" unit (fun () -> Id) in
      let prompt = constr "Prompt" unit (fun () -> Prompt) in
      let hint = constr "Hint" unit (fun () -> Hint) in
      let details = constr "Details" unit (fun () -> Details) in
      let ok = constr "Ok" unit (fun () -> Ok) in
      let debug = constr "Debug" unit (fun () -> Debug) in
      let success = constr "Success" unit (fun () -> Success) in
      let ansi_styles =
        constr "Ansi_styles" (list Ansi_color.Style.sexp) (fun l -> Ansi_styles l)
      in
      sum
        [ econstr loc
        ; econstr error
        ; econstr warning
        ; econstr kwd
        ; econstr id
        ; econstr prompt
        ; econstr hint
        ; econstr details
        ; econstr ok
        ; econstr debug
        ; econstr success
        ; econstr ansi_styles
        ]
        (function
          | Loc -> case () loc
          | Error -> case () error
          | Warning -> case () warning
          | Kwd -> case () kwd
          | Id -> case () id
          | Prompt -> case () prompt
          | Hint -> case () hint
          | Details -> case () details
          | Ok -> case () ok
          | Debug -> case () debug
          | Success -> case () success
          | Ansi_styles l -> case l ansi_styles)
    ;;
  end
end

module Target = struct
  type t =
    | Path of string
    | Alias of string
    | Library of string
    | Executables of string list
    | Preprocess of string list
    | Loc of Loc.t

  let sexp =
    let open Conv in
    let path = constr "Path" string (fun p -> Path p) in
    let alias = constr "Alias" string (fun a -> Alias a) in
    let lib = constr "Library" string (fun l -> Library l) in
    let executables = constr "Executables" (list string) (fun es -> Executables es) in
    let preprocess = constr "Preprocess" (list string) (fun ps -> Preprocess ps) in
    let loc = constr "Loc" Loc.sexp (fun l -> Loc l) in
    sum
      [ econstr path
      ; econstr alias
      ; econstr lib
      ; econstr executables
      ; econstr preprocess
      ; econstr loc
      ]
      (function
        | Path p -> case p path
        | Alias a -> case a alias
        | Library l -> case l lib
        | Executables es -> case es executables
        | Preprocess ps -> case ps preprocess
        | Loc l -> case l loc)
  ;;
end

module Path = struct
  type t = string

  let sexp = Conv.string
  let dune_root = "."
  let to_string_absolute x = x

  let absolute abs =
    if Filename.is_relative abs
    then
      Code_error.raise
        "Path.absolute: accepts only absolute paths"
        [ "abs", Dyn.string abs ];
    abs
  ;;

  let relative = Filename.concat
end

(* This has a subtle difference with [sexp_pp] in how we serialise tags. *)
let sexp_pp_unit : unit Pp.t Conv.value =
  let open Conv in
  let open Pp.Ast in
  let nop = constr "Nop" unit (fun () -> Nop) in
  let verbatim = constr "Verbatim" string (fun s -> Verbatim s) in
  let char = constr "Char" char (fun c -> Char c) in
  let newline = constr "Newline" unit (fun () -> Newline) in
  let t =
    fixpoint (fun t ->
      let text = constr "Text" string (fun s -> Text s) in
      let seq = constr "Seq" (pair t t) (fun (x, y) -> Seq (x, y)) in
      let concat = constr "Concat" (pair t (list t)) (fun (x, y) -> Concat (x, y)) in
      let box = constr "Box" (pair int t) (fun (x, y) -> Box (x, y)) in
      let vbox = constr "Vbox" (pair int t) (fun (x, y) -> Vbox (x, y)) in
      let hbox = constr "Hbox" t (fun t -> Hbox t) in
      let hvbox = constr "Hvbox" (pair int t) (fun (x, y) -> Hvbox (x, y)) in
      let hovbox = constr "Hovbox" (pair int t) (fun (x, y) -> Hovbox (x, y)) in
      let break =
        constr
          "Break"
          (pair (triple string int string) (triple string int string))
          (fun (x, y) -> Break (x, y))
      in
      let tag = constr "Tag" t (fun t -> Tag ((), t)) in
      sum
        [ econstr nop
        ; econstr verbatim
        ; econstr char
        ; econstr newline
        ; econstr text
        ; econstr seq
        ; econstr concat
        ; econstr box
        ; econstr vbox
        ; econstr hbox
        ; econstr hvbox
        ; econstr hovbox
        ; econstr break
        ; econstr tag
        ]
        (function
          | Nop -> case () nop
          | Seq (x, y) -> case (x, y) seq
          | Concat (x, y) -> case (x, y) concat
          | Box (i, t) -> case (i, t) box
          | Vbox (i, t) -> case (i, t) vbox
          | Hbox t -> case t hbox
          | Hvbox (i, t) -> case (i, t) hvbox
          | Hovbox (i, t) -> case (i, t) hovbox
          | Verbatim s -> case s verbatim
          | Char c -> case c char
          | Break (x, y) -> case (x, y) break
          | Newline -> case () newline
          | Text s -> case s text
          | Tag ((), t) -> case t tag))
  in
  let to_ast x =
    match Pp.to_ast x with
    | Ok s -> s
    | Error () ->
      (* We don't use the format constructor in dune. *)
      assert false
  in
  iso t Pp.of_ast to_ast
;;

module Diagnostic = struct
  type severity =
    | Error
    | Warning

  module Promotion = struct
    type t =
      { in_build : string
      ; in_source : string
      }

    let in_build t = t.in_build
    let in_source t = t.in_source

    let sexp =
      let open Conv in
      let from { in_build; in_source } = in_build, in_source in
      let to_ (in_build, in_source) = { in_build; in_source } in
      let in_build = field "in_build" (required string) in
      let in_source = field "in_source" (required string) in
      iso (record (both in_build in_source)) to_ from
    ;;
  end

  let sexp_pp (conv_tag : 'a Conv.value) : 'a Pp.t Conv.value =
    let open Conv in
    let open Pp.Ast in
    let nop = constr "Nop" unit (fun () -> Nop) in
    let verbatim = constr "Verbatim" string (fun s -> Verbatim s) in
    let char = constr "Char" char (fun c -> Char c) in
    let newline = constr "Newline" unit (fun () -> Newline) in
    let t =
      fixpoint (fun t ->
        let text = constr "Text" string (fun s -> Text s) in
        let seq = constr "Seq" (pair t t) (fun (x, y) -> Seq (x, y)) in
        let concat = constr "Concat" (pair t (list t)) (fun (x, y) -> Concat (x, y)) in
        let box = constr "Box" (pair int t) (fun (x, y) -> Box (x, y)) in
        let vbox = constr "Vbox" (pair int t) (fun (x, y) -> Vbox (x, y)) in
        let hbox = constr "Hbox" t (fun t -> Hbox t) in
        let hvbox = constr "Hvbox" (pair int t) (fun (x, y) -> Hvbox (x, y)) in
        let hovbox = constr "Hovbox" (pair int t) (fun (x, y) -> Hovbox (x, y)) in
        let break =
          constr
            "Break"
            (pair (triple string int string) (triple string int string))
            (fun (x, y) -> Break (x, y))
        in
        let tag = constr "Tag" (pair conv_tag t) (fun (s, t) -> Tag (s, t)) in
        sum
          [ econstr nop
          ; econstr verbatim
          ; econstr char
          ; econstr newline
          ; econstr text
          ; econstr seq
          ; econstr concat
          ; econstr box
          ; econstr vbox
          ; econstr hbox
          ; econstr hvbox
          ; econstr hovbox
          ; econstr break
          ; econstr tag
          ]
          (function
            | Nop -> case () nop
            | Seq (x, y) -> case (x, y) seq
            | Concat (x, y) -> case (x, y) concat
            | Box (i, t) -> case (i, t) box
            | Vbox (i, t) -> case (i, t) vbox
            | Hbox t -> case t hbox
            | Hvbox (i, t) -> case (i, t) hvbox
            | Hovbox (i, t) -> case (i, t) hovbox
            | Verbatim s -> case s verbatim
            | Char c -> case c char
            | Break (x, y) -> case (x, y) break
            | Newline -> case () newline
            | Text s -> case s text
            | Tag (s, t) -> case (s, t) tag))
    in
    let to_ast x =
      match Pp.to_ast x with
      | Ok s -> s
      | Error () ->
        (* We don't use the format constructor in dune. *)
        assert false
    in
    iso t Pp.of_ast to_ast
  ;;

  module Id = struct
    type t = int

    let compare (a : t) (b : t) = Int.compare a b
    let hash (t : t) = Hashtbl.hash t
    let create t : t = t
    let sexp = Conv.int
  end

  module Related = struct
    type t =
      { message : User_message.Style.t Pp.t
      ; loc : Loc.t
      }

    let message t = t.message |> Pp.map_tags ~f:(fun _ -> ())
    let message_with_style t = t.message
    let loc t = t.loc

    let sexp =
      let open Conv in
      let loc = field "loc" (required Loc.sexp) in
      let message = field "message" (required (sexp_pp User_message.Style.sexp)) in
      let to_ (loc, message) = { loc; message } in
      let from { loc; message } = loc, message in
      iso (record (both loc message)) to_ from
    ;;
  end

  type t =
    { targets : Target.t list
    ; id : Id.t
    ; message : User_message.Style.t Pp.t
    ; loc : Loc.t option
    ; severity : severity option
    ; promotion : Promotion.t list
    ; directory : string option
    ; related : Related.t list
    }

  let loc t = t.loc
  let message t = t.message |> Pp.map_tags ~f:(fun _ -> ())
  let message_with_style t = t.message
  let severity t = t.severity
  let promotion t = t.promotion
  let targets t = t.targets
  let directory t = t.directory
  let related t = t.related
  let id t = t.id

  let sexp_severity =
    let open Conv in
    enum [ "error", Error; "warning", Warning ]
  ;;

  let sexp =
    let open Conv in
    let from { targets; message; loc; severity; promotion; directory; id; related } =
      targets, message, loc, severity, promotion, directory, id, related
    in
    let to_ (targets, message, loc, severity, promotion, directory, id, related) =
      { targets; message; loc; severity; promotion; directory; id; related }
    in
    let loc = field "loc" (optional Loc.sexp) in
    let message = field "message" (required (sexp_pp User_message.Style.sexp)) in
    let targets = field "targets" (required (list Target.sexp)) in
    let severity = field "severity" (optional sexp_severity) in
    let directory = field "directory" (optional string) in
    let promotion = field "promotion" (required (list Promotion.sexp)) in
    let id = field "id" (required Id.sexp) in
    let related = field "related" (required (list Related.sexp)) in
    iso
      (record (eight targets message loc severity promotion directory id related))
      to_
      from
  ;;

  let to_dyn t = Sexp.to_dyn (Conv.to_sexp sexp t)

  let to_user_message t =
    let loc = Option.map t.loc ~f:Stdune.Loc.of_lexbuf_loc in
    Stdune.User_message.make ?loc [ t.message ]
  ;;

  module Event = struct
    type nonrec t =
      | Add of t
      | Remove of t

    let sexp =
      let diagnostic = sexp in
      let open Conv in
      let add = constr "Add" diagnostic (fun a -> Add a) in
      let remove = constr "Remove" diagnostic (fun a -> Remove a) in
      sum
        [ econstr add; econstr remove ]
        (function
          | Add t -> case t add
          | Remove t -> case t remove)
    ;;

    let to_dyn t = Sexp.to_dyn (Conv.to_sexp sexp t)
  end
end

module Progress = struct
  type t =
    | Waiting
    | In_progress of
        { complete : int
        ; remaining : int
        ; failed : int
        }
    | Failed
    | Interrupted
    | Success

  let sexp =
    let open Conv in
    let waiting = constr "waiting" unit (fun () -> Waiting) in
    let failed = constr "failed" unit (fun () -> Failed) in
    let in_progress =
      let complete = field "complete" (required int) in
      let remaining = field "remaining" (required int) in
      let failed = field "failed" (required int) in
      constr
        "in_progress"
        (record (three complete remaining failed))
        (fun (complete, remaining, failed) -> In_progress { complete; remaining; failed })
    in
    let interrupted = constr "interrupted" unit (fun () -> Interrupted) in
    let success = constr "success" unit (fun () -> Success) in
    let constrs =
      List.map ~f:econstr [ waiting; failed; interrupted; success ]
      @ [ econstr in_progress ]
    in
    let serialize = function
      | Waiting -> case () waiting
      | In_progress { complete; remaining; failed } ->
        case (complete, remaining, failed) in_progress
      | Failed -> case () failed
      | Interrupted -> case () interrupted
      | Success -> case () success
    in
    sum constrs serialize
  ;;
end

module Message = struct
  type t =
    { payload : Sexp.t option
    ; message : string
    }

  let payload t = t.payload
  let message t = t.message

  let sexp =
    let open Conv in
    let from { payload; message } = payload, message in
    let to_ (payload, message) = { payload; message } in
    let payload = field "payload" (optional sexp) in
    let message = field "message" (required string) in
    iso (record (both payload message)) to_ from
  ;;

  let to_sexp_unversioned = Conv.to_sexp sexp
end

module Job = struct
  module Id = Diagnostic.Id

  type t =
    { id : Id.t
    ; pid : int
    ; description : unit Pp.t
    ; started_at : float
    }

  let id t = t.id
  let pid t = t.pid
  let description t = t.description
  let started_at t = t.started_at

  let sexp =
    let open Conv in
    let from { id; pid; description; started_at } = id, pid, description, started_at in
    let to_ (id, pid, description, started_at) = { id; pid; description; started_at } in
    let id = field "id" (required Id.sexp) in
    let started_at = field "started_at" (required float) in
    let pid = field "pid" (required int) in
    let description = field "description" (required sexp_pp_unit) in
    iso (record (four id pid description started_at)) to_ from
  ;;

  module Event = struct
    type nonrec t =
      | Start of t
      | Stop of Id.t

    let sexp =
      let job = sexp in
      let open Conv in
      let start = constr "Start" job (fun a -> Start a) in
      let stop = constr "Stop" Id.sexp (fun a -> Stop a) in
      sum
        [ econstr start; econstr stop ]
        (function
          | Start t -> case t start
          | Stop t -> case t stop)
    ;;
  end
end
