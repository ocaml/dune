open Import
open Decoder

module T = struct
  type t =
    | Byte
    | Native
    | Best

  let all = [ Byte; Native; Best ]
end

include T

let repr =
  Repr.variant
    "mode-conf"
    [ Repr.case0 "byte" ~test:(function
        | Byte -> true
        | Native | Best -> false)
    ; Repr.case0 "native" ~test:(function
        | Native -> true
        | Byte | Best -> false)
    ; Repr.case0 "best" ~test:(function
        | Best -> true
        | Byte | Native -> false)
    ]
;;

include Repr.Poly (struct
    type nonrec t = t

    let repr = repr
  end)

let decode = enum [ "byte", Byte; "native", Native; "best", Best ]

let to_string = function
  | Byte -> "byte"
  | Native -> "native"
  | Best -> "best"
;;

let to_dyn = Repr.to_dyn repr
let encode t = Dune_sexp.atom (to_string t)

module Kind = struct
  type t =
    | Inherited
    | Requested of Loc.t
end

module Map = struct
  type nonrec 'a t =
    { byte : 'a
    ; native : 'a
    ; best : 'a
    }

  let find t = function
    | Byte -> t.byte
    | Native -> t.native
    | Best -> t.best
  ;;

  let update t key ~f =
    match key with
    | Byte -> { t with byte = f t.byte }
    | Native -> { t with native = f t.native }
    | Best -> { t with best = f t.best }
  ;;

  let make_one x = { byte = x; native = x; best = x }
end

type mode_conf = t

module Set = struct
  type nonrec t = Kind.t option Map.t

  let empty : t = Map.make_one None

  let of_list (input : (mode_conf * Kind.t) list) : t =
    List.fold_left ~init:empty input ~f:(fun acc (key, kind) ->
      Map.update acc key ~f:(function
        | None -> Some kind
        | Some (Kind.Requested loc) ->
          User_error.raise ~loc [ Pp.textf "already configured" ]
        | Some Inherited ->
          (* this doesn't happen as inherited can't be manually specified *)
          assert false))
  ;;

  let to_list (t : t) : (mode_conf * Kind.t) list =
    let get mode_conf =
      match Map.find t mode_conf with
      | None -> None
      | Some k -> Some (mode_conf, k)
    in
    List.filter_map ~f:get all
  ;;

  let decode =
    let decode =
      let+ loc, t = located decode in
      t, Kind.Requested loc
    in
    repeat decode >>| of_list
  ;;

  let default loc : t = { empty with byte = Some Inherited; best = Some (Requested loc) }

  let eval t ~has_native =
    let exists = function
      | Best | Byte -> true
      | Native -> has_native
    in
    let get key : bool =
      match Map.find t key with
      | None -> false
      | Some Kind.Inherited -> exists key
      | Some (Kind.Requested loc) ->
        (* TODO always true for now, but we should delay this error *)
        exists key || User_error.raise ~loc [ Pp.text "this mode isn't available" ]
    in
    let best_mode = if has_native then Native else Byte in
    let best = get Best in
    let byte = get Byte || if best_mode = Byte then best else false in
    let native = get Native || if best_mode = Native then best else false in
    { Mode.Dict.byte; native }
  ;;
end

module Lib = struct
  type t =
    | Ocaml of mode_conf
    | Melange

  let repr =
    Repr.variant
      "mode-conf-lib"
      [ Repr.case0 "byte" ~test:(function
          | Ocaml Byte -> true
          | Ocaml (Native | Best) | Melange -> false)
      ; Repr.case0 "native" ~test:(function
          | Ocaml Native -> true
          | Ocaml (Byte | Best) | Melange -> false)
      ; Repr.case0 "best" ~test:(function
          | Ocaml Best -> true
          | Ocaml (Byte | Native) | Melange -> false)
      ; Repr.case0 "melange" ~test:(function
          | Melange -> true
          | Ocaml _ -> false)
      ]
  ;;

  include Repr.Poly (struct
      type nonrec t = t

      let repr = repr
    end)

  let decode =
    enum'
      [ "byte", return @@ Ocaml Byte
      ; "native", return @@ Ocaml Native
      ; "best", return @@ Ocaml Best
      ; "melange", Syntax.since Melange.syntax (0, 1) >>> return Melange
      ]
  ;;

  let to_dyn = Repr.to_dyn repr

  module Map = struct
    type nonrec 'a t =
      { ocaml : 'a Map.t
      ; melange : 'a
      }

    let find t = function
      | Ocaml a -> Map.find t.ocaml a
      | Melange -> t.melange
    ;;

    let update t key ~f =
      match key with
      | Ocaml key -> { t with ocaml = Map.update t.ocaml key ~f }
      | Melange -> { t with melange = f t.melange }
    ;;

    let make_one x = { ocaml = Map.make_one x; melange = x }
  end

  module Set = struct
    type mode_conf = t
    type nonrec t = Kind.t option Map.t

    let empty : t = Map.make_one None

    let of_list (input : (mode_conf * Kind.t) list) : t =
      List.fold_left ~init:empty input ~f:(fun acc (key, kind) ->
        Map.update acc key ~f:(function
          | None -> Some kind
          | Some (Kind.Requested loc) ->
            User_error.raise ~loc [ Pp.textf "already configured" ]
          | Some Inherited ->
            (* this doesn't happen as inherited can't be manually specified *)
            assert false))
    ;;

    let decode_osl ~stanza_loc project =
      let+ modes = Ordered_set_lang.decode in
      let standard =
        Set.default stanza_loc |> Set.to_list |> List.map ~f:(fun (m, k) -> Ocaml m, k)
      in
      Ordered_set_lang.eval
        modes
        ~standard
        ~eq:(fun (a, _) (b, _) -> equal a b)
        ~parse:(fun ~loc s ->
          let mode =
            Decoder.parse
              (Dune_project.set_parsing_context project decode)
              Univ_map.empty
              (Atom (loc, Dune_sexp.Atom.of_string s))
          in
          mode, Kind.Requested loc)
      |> of_list
    ;;

    let decode =
      let decode =
        let+ loc, t = located decode in
        t, Kind.Requested loc
      in
      repeat decode >>| of_list
    ;;

    let default loc : t = { empty with ocaml = Set.default loc }

    let eval t ~has_native =
      let melange = Map.find t Melange |> Option.is_some in
      { Lib_mode.Map.ocaml = Set.eval t.ocaml ~has_native; melange }
    ;;
  end
end
