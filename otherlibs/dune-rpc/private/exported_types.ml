open Import

module Loc = struct
  include Loc

  let start t = t.start

  let stop t = t.stop

  let pos_sexp =
    let open Conv in
    let to_ (pos_fname, pos_lnum, pos_bol, pos_cnum) =
      { Lexing.pos_fname; pos_lnum; pos_bol; pos_cnum }
    in
    let from { Lexing.pos_fname; pos_lnum; pos_bol; pos_cnum } =
      (pos_fname, pos_lnum, pos_bol, pos_cnum)
    in
    let pos_fname = field "pos_fname" (required string) in
    let pos_lnum = field "pos_lnum" (required int) in
    let pos_bol = field "pos_bol" (required int) in
    let pos_cnum = field "pos_cnum" (required int) in
    iso (record (four pos_fname pos_lnum pos_bol pos_cnum)) to_ from

  let sexp =
    let open Conv in
    let to_ (start, stop) = { start; stop } in
    let from { start; stop } = (start, stop) in
    let start = field "start" (required pos_sexp) in
    let stop = field "stop" (required pos_sexp) in
    iso (record (both start stop)) to_ from
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
    let executables =
      constr "Executables" (list string) (fun es -> Executables es)
    in
    let preprocess =
      constr "Preprocess" (list string) (fun ps -> Preprocess ps)
    in
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
end

module Path = struct
  type t = string

  let sexp = Conv.string

  let dune_root = "."

  let to_string_absolute x = x

  let absolute abs =
    if Filename.is_relative abs then
      Code_error.raise "Path.absolute: accepts only absolute paths"
        [ ("abs", Dyn.string abs) ];
    abs

  let relative = Filename.concat
end

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
      let from { in_build; in_source } = (in_build, in_source) in
      let to_ (in_build, in_source) = { in_build; in_source } in
      let in_build = field "in_build" (required string) in
      let in_source = field "in_source" (required string) in
      iso (record (both in_build in_source)) to_ from
  end

  let sexp_pp : (unit Pp.t, Conv.values) Conv.t =
    let open Conv in
    let open Pp.Ast in
    let nop = constr "Nop" unit (fun () -> Nop) in
    let verbatim = constr "Verbatim" string (fun s -> Verbatim s) in
    let char = constr "Char" char (fun c -> Char c) in
    let newline = constr "Newline" unit (fun () -> Newline) in
    let t_fdecl = Fdecl.create Dyn.opaque in
    let t = fdecl t_fdecl in
    let text = constr "Text" string (fun s -> Text s) in
    let seq = constr "Seq" (pair t t) (fun (x, y) -> Seq (x, y)) in
    let concat =
      constr "Concat" (pair t (list t)) (fun (x, y) -> Concat (x, y))
    in
    let box = constr "Box" (pair int t) (fun (x, y) -> Box (x, y)) in
    let vbox = constr "Vbox" (pair int t) (fun (x, y) -> Vbox (x, y)) in
    let hbox = constr "Hbox" t (fun t -> Hbox t) in
    let hvbox = constr "Hvbox" (pair int t) (fun (x, y) -> Hvbox (x, y)) in
    let hovbox = constr "Hovbox" (pair int t) (fun (x, y) -> Hovbox (x, y)) in
    let break =
      constr "Break"
        (pair (triple string int string) (triple string int string))
        (fun (x, y) -> Break (x, y))
    in
    let tag = constr "Tag" t (fun t -> Tag ((), t)) in
    let conv =
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
          | Tag ((), t) -> case t tag)
    in
    Fdecl.set t_fdecl conv;
    let to_ast x =
      match Pp.to_ast x with
      | Ok s -> s
      | Error () ->
        (* We don't use the format constructor in dune. *)
        assert false
    in
    iso (Fdecl.get t_fdecl) Pp.of_ast to_ast

  module Id = struct
    type t = int

    let compare (a : t) (b : t) = Int.compare a b

    let hash (t : t) = Hashtbl.hash t

    let create t : t = t

    let sexp = Conv.int
  end

  module Related = struct
    type t =
      { message : unit Pp.t
      ; loc : Loc.t
      }

    let message t = t.message

    let loc t = t.loc

    let sexp =
      let open Conv in
      let loc = field "loc" (required Loc.sexp) in
      let message = field "message" (required sexp_pp) in
      let to_ (loc, message) = { loc; message } in
      let from { loc; message } = (loc, message) in
      iso (record (both loc message)) to_ from
  end

  type t =
    { targets : Target.t list
    ; id : Id.t
    ; message : unit Pp.t
    ; loc : Loc.t option
    ; severity : severity option
    ; promotion : Promotion.t list
    ; directory : string option
    ; related : Related.t list
    }

  let loc t = t.loc

  let message t = t.message

  let severity t = t.severity

  let promotion t = t.promotion

  let targets t = t.targets

  let directory t = t.directory

  let related t = t.related

  let id t = t.id

  let sexp_severity =
    let open Conv in
    enum [ ("error", Error); ("warning", Warning) ]

  let sexp =
    let open Conv in
    let from
        { targets; message; loc; severity; promotion; directory; id; related } =
      (targets, message, loc, severity, promotion, directory, id, related)
    in
    let to_ (targets, message, loc, severity, promotion, directory, id, related)
        =
      { targets; message; loc; severity; promotion; directory; id; related }
    in
    let loc = field "loc" (optional Loc.sexp) in
    let message = field "message" (required sexp_pp) in
    let targets = field "targets" (required (list Target.sexp)) in
    let severity = field "severity" (optional sexp_severity) in
    let directory = field "directory" (optional string) in
    let promotion = field "promotion" (required (list Promotion.sexp)) in
    let id = field "id" (required Id.sexp) in
    let related = field "related" (required (list Related.sexp)) in
    iso
      (record
         (eight targets message loc severity promotion directory id related))
      to_ from

  let to_dyn t = Sexp.to_dyn (Conv.to_sexp sexp t)

  let to_user_message t =
    let prefix =
      Option.map t.severity ~f:(fun sev ->
          let severity, prefix =
            match sev with
            | Error -> (Stdune.User_message.Style.Error, "Error:")
            | Warning -> (Warning, "Warning:")
          in
          Pp.tag severity (Pp.text prefix))
    in
    let directory =
      match t.directory with
      | None -> []
      | Some d ->
        [ Pp.tag Stdune.User_message.Style.Loc (Pp.textf "(In directory %s)" d)
        ]
    in
    let formatted_loc =
      match t.loc with
      | None -> []
      | Some l ->
        [ Pp.map_tags ~f:(fun _ -> Stdune.User_message.Style.Loc) (Loc.pp l) ]
    in
    Stdune.User_message.make ?prefix
      (directory @ formatted_loc
      @ [ Pp.map_tags ~f:(fun _ -> Stdune.User_message.Style.Details) t.message
        ])

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

    let to_dyn t = Sexp.to_dyn (Conv.to_sexp sexp t)
  end
end

module Progress = struct
  type t =
    | Waiting
    | In_progress of
        { complete : int
        ; remaining : int
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
      constr "in_progress"
        (record (both complete remaining))
        (fun (complete, remaining) -> In_progress { complete; remaining })
    in
    let interrupted = constr "interrupted" unit (fun () -> Interrupted) in
    let success = constr "success" unit (fun () -> Success) in
    let constrs =
      List.map ~f:econstr [ waiting; failed; interrupted; success ]
      @ [ econstr in_progress ]
    in
    let serialize = function
      | Waiting -> case () waiting
      | In_progress { complete; remaining } ->
        case (complete, remaining) in_progress
      | Failed -> case () failed
      | Interrupted -> case () interrupted
      | Success -> case () success
    in
    sum constrs serialize
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
    let from { payload; message } = (payload, message) in
    let to_ (payload, message) = { payload; message } in
    let payload = field "payload" (optional sexp) in
    let message = field "message" (required string) in
    iso (record (both payload message)) to_ from

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
    let from { id; pid; description; started_at } =
      (id, pid, description, started_at)
    in
    let to_ (id, pid, description, started_at) =
      { id; pid; description; started_at }
    in
    let id = field "id" (required Id.sexp) in
    let started_at = field "started_at" (required float) in
    let pid = field "pid" (required int) in
    let description = field "description" (required Diagnostic.sexp_pp) in
    iso (record (four id pid description started_at)) to_ from

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
  end
end
