open! Stdune

module Loc = struct
  module Position = struct
    (** Lexing.position without filename *)
    type t =
      { lnum : int
      ; cnum : int
      ; bol : int
      }

    let to_sexp { lnum; cnum; bol } =
      Sexp.List
        [ Atom (Int.to_string lnum)
        ; Atom (Int.to_string cnum)
        ; Atom (Int.to_string bol)
        ]

    let of_sexp sexp =
      match sexp with
      | Sexp.List [ Atom lnum; Atom cnum; Atom bol ] -> (
        match (Int.of_string lnum, Int.of_string cnum, Int.of_string bol) with
        | Some lnum, Some cnum, Some bol -> { lnum; cnum; bol }
        | _ ->
          User_error.raise
            [ Pp.textf "Cannot parse Position ints: (%s,%s,%s)." lnum cnum bol ]
        )
      | _ -> User_error.raise [ Pp.textf "Cannot parse Position." ]

    let to_pos fname { lnum; cnum; bol } : Lexing.position =
      { pos_bol = bol; pos_cnum = cnum; pos_lnum = lnum; pos_fname = fname }

    let of_pos { Lexing.pos_bol; pos_cnum; pos_lnum; _ } : t =
      { lnum = pos_lnum; cnum = pos_cnum; bol = pos_bol }
  end

  type t =
    { start : Position.t
    ; stop : Position.t
    }

  let to_sexp { start; stop } =
    Sexp.List [ Position.to_sexp start; Position.to_sexp stop ]

  let of_sexp sexp : t =
    match sexp with
    | Sexp.List [ start; stop ] ->
      { start = Position.of_sexp start; stop = Position.of_sexp stop }
    | _ -> User_error.raise Pp.[ textf "Cannot parse Loc." ]

  let to_loc { start; stop } ~fname : Stdune.Loc.t =
    { start = Position.to_pos fname start; stop = Position.to_pos fname stop }

  let of_loc { Stdune.Loc.stop; start } : t =
    { start = Position.of_pos start; stop = Position.of_pos stop }

  let of_lexbuf lexbuf = of_loc @@ Stdune.Loc.of_lexbuf lexbuf
end

module Module = struct
  type t =
    { loc : Loc.t
    ; logical_name : string
    }

  let loc t = t.loc

  let name t = t.logical_name

  let make loc logical_name = { loc; logical_name }

  let to_sexp t : Csexp.t = List [ Loc.to_sexp t.loc; Atom t.logical_name ]

  let of_sexp sexp =
    let open Csexp in
    match sexp with
    | List [ loc_sexp; Atom logical_name ] ->
      { loc = Loc.of_sexp loc_sexp; logical_name }
    | _ -> User_error.raise Pp.[ textf "Cannot parse Module." ]

  let compare x y = String.compare x.logical_name y.logical_name
end

module Ml_module = struct
  include Module

  let of_sexp sexp =
    let open Csexp in
    match sexp with
    | List [ loc_sexp; Atom logical_name ] ->
      { loc = Loc.of_sexp loc_sexp; logical_name }
    | _ -> User_error.raise Pp.[ textf "Cannot parse Ml_module." ]
end

module From = struct
  type t =
    { prefix : Module.t option
    ; require : Module.t
    }

  let prefix t = t.prefix

  let require t = t.require

  let make prefix require = { prefix; require }

  let to_sexp t =
    let open Csexp in
    match t.prefix with
    | None -> List [ Module.to_sexp t.require ]
    | Some prefix -> List [ Module.to_sexp prefix; Module.to_sexp t.require ]

  let of_sexp csexp : t =
    let open Csexp in
    match csexp with
    | List [ require_sexp ] ->
      { prefix = None; require = Module.of_sexp require_sexp }
    | List [ prefix_sexp; require_sexp ] ->
      { prefix = Some (Module.of_sexp prefix_sexp)
      ; require = Module.of_sexp require_sexp
      }
    | _ -> User_error.raise Pp.[ textf "Cannot parse From." ]

  let compare x y =
    (* When comparing we ignore the locations *)
    if
      Option.equal
        Module.(fun x y -> x.logical_name = y.logical_name)
        x.prefix y.prefix
    then Module.compare x.require y.require
    else Option.compare Module.compare x.prefix y.prefix
end

module Load = struct
  type t =
    { loc : Loc.t
    ; path : string
    }

  let loc t = t.loc

  let path t = t.path

  let make loc path = { loc; path }

  let to_sexp t =
    let open Csexp in
    List [ Loc.to_sexp t.loc; Atom t.path ]

  let of_sexp sexp =
    let open Csexp in
    match sexp with
    | List [ loc_sexp; Atom path ] -> { loc = Loc.of_sexp loc_sexp; path }
    | _ -> User_error.raise Pp.[ textf "Cannot parse Load." ]

  let compare x y = String.compare x.path y.path
end

module ExtraDep = struct
  type t =
    { loc : Loc.t
    ; from : Module.t
    ; file : string
    }

  let loc t = t.loc

  let from t = t.from

  let file t = t.file

  let make loc from file = { loc; from; file }

  let to_sexp t =
    let open Csexp in
    List [ Module.to_sexp t.from; Loc.to_sexp t.loc; Atom t.file ]

  let of_sexp sexp =
    let open Csexp in
    match sexp with
    | List [ from_sexp; loc_sexp; Atom file ] ->
      { from = Module.of_sexp from_sexp; loc = Loc.of_sexp loc_sexp; file }
    | _ -> User_error.raise Pp.[ textf "Cannot parse ExtraDep." ]

  let compare x y =
    let open Module in
    if x.from.logical_name = y.from.logical_name then
      String.compare x.file y.file
    else Module.compare x.from y.from
end

type t =
  { froms : From.t list
  ; declares : Ml_module.t list
  ; loads : Load.t list
  ; extradeps : ExtraDep.t list
  }

let froms t = t.froms

let declares t = t.declares

let loads t = t.loads

let extradeps t = t.extradeps

let empty = { froms = []; declares = []; loads = []; extradeps = [] }

let add_from t prefix require =
  { t with froms = From.make prefix require :: t.froms }

let add_from_list t prefix requires =
  let froms = List.map ~f:(fun require -> From.make prefix require) requires in
  { t with froms = froms @ t.froms }

let add_require t require = add_from t None require

let add_require_list t requires = add_from_list t None requires

let add_declare_list t declares = { t with declares = declares @ t.declares }

let add_load t loc path =
  { t with loads = Load.make (Loc.of_loc loc) path :: t.loads }

let add_extrdep t loc from file =
  { t with extradeps = ExtraDep.make (Loc.of_loc loc) from file :: t.extradeps }

let sexp_of_declares declares =
  Csexp.[ List (List.map ~f:Module.to_sexp declares) ]

let sexp_of_froms froms = Csexp.[ List (List.map ~f:From.to_sexp froms) ]

let sexp_of_loads loads = Csexp.[ List (List.map ~f:Load.to_sexp loads) ]

let sexp_extradeps extradeps =
  Csexp.[ List (List.map ~f:ExtraDep.to_sexp extradeps) ]

let to_sexp { froms; declares; loads; extradeps } =
  let list f xs = Csexp.List (ListLabels.map ~f xs) in
  Csexp.List
    [ list From.to_sexp froms
    ; list Ml_module.to_sexp declares
    ; list Load.to_sexp loads
    ; list ExtraDep.to_sexp extradeps
    ]

let of_sexp (sexp : Csexp.t) =
  match sexp with
  | List [ froms; declares; loads; extradeps ] ->
    let list conv (sexp : Sexp.t) =
      match sexp with
      | List xs -> List.map ~f:conv xs
      | _ -> User_error.raise [ Pp.text "Cannot parse Deps list." ]
    in
    let froms = list From.of_sexp froms in
    let declares = list Module.of_sexp declares in
    let loads = list Load.of_sexp loads in
    let extradeps = list ExtraDep.of_sexp extradeps in
    { froms; loads; declares; extradeps }
  | _ -> User_error.raise [ Pp.text "Cannot parse Deps." ]

let sort_uniq t =
  { froms = List.sort_uniq ~compare:From.compare t.froms
  ; declares = List.sort_uniq ~compare:Module.compare t.declares
  ; loads = List.sort_uniq ~compare:Load.compare t.loads
  ; extradeps = List.sort_uniq ~compare:ExtraDep.compare t.extradeps
  }
