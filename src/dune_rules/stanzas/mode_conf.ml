open Import
open Dune_lang.Decoder

module T = struct
  type t =
    | Byte
    | Native
    | Best

  let all = [ Byte; Native; Best ]

  let compare x y =
    match x, y with
    | Byte, Byte -> Eq
    | Byte, _ -> Lt
    | _, Byte -> Gt
    | Native, Native -> Eq
    | Native, _ -> Lt
    | _, Native -> Gt
    | Best, Best -> Eq
  ;;
end

include T

let decode = enum [ "byte", Byte; "native", Native; "best", Best ]

let to_string = function
  | Byte -> "byte"
  | Native -> "native"
  | Best -> "best"
;;

let to_dyn t = Dyn.variant (to_string t) []
let encode t = Dune_lang.atom (to_string t)

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

  module Details = struct
    type t = Kind.t option

    let validate t ~if_ = if if_ then t else None
    let ( ||| ) x y = if Option.is_some x then x else y
  end

  let eval_detailed t ~has_native =
    let exists = function
      | Best | Byte -> true
      | Native -> has_native
    in
    let get key : Details.t =
      match Map.find t key with
      | None -> None
      | Some Kind.Inherited -> Option.some_if (exists key) Kind.Inherited
      | Some (Kind.Requested loc) ->
        (* TODO always true for now, but we should delay this error *)
        let exists =
          exists key || User_error.raise ~loc [ Pp.text "this mode isn't available" ]
        in
        Option.some_if exists (Kind.Requested loc)
    in
    let best_mode = if has_native then Native else Byte in
    let best = get Best in
    let open Details in
    let byte = get Byte ||| validate best ~if_:(best_mode = Byte) in
    let native = get Native ||| validate best ~if_:(best_mode = Native) in
    { Mode.Dict.byte; native }
  ;;

  let eval t ~has_native = eval_detailed t ~has_native |> Mode.Dict.map ~f:Option.is_some
end

module Lib = struct
  type t =
    | Ocaml of mode_conf
    | Melange

  let decode =
    enum'
      [ "byte", return @@ Ocaml Byte
      ; "native", return @@ Ocaml Native
      ; "best", return @@ Ocaml Best
      ; "melange", Dune_lang.Syntax.since Melange_stanzas.syntax (0, 1) >>> return Melange
      ]
  ;;

  let to_string = function
    | Ocaml Byte -> "byte"
    | Ocaml Native -> "native"
    | Ocaml Best -> "best"
    | Melange -> "melange"
  ;;

  let to_dyn t = Dyn.variant (to_string t) []

  let equal x y =
    match x, y with
    | Ocaml o1, Ocaml o2 ->
      (match compare o1 o2 with
       | Eq -> true
       | _ -> false)
    | Ocaml _, _ | _, Ocaml _ -> false
    | Melange, Melange -> true
  ;;

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
            Dune_lang.Decoder.parse
              (Dune_project.set_parsing_context project decode)
              Univ_map.empty
              (Atom (loc, Dune_lang.Atom.of_string s))
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

    module Details = struct
      type t = Kind.t option
    end

    let eval_detailed t ~has_native =
      let get key : Details.t = Map.find t key in
      let melange = get Melange in
      { Lib_mode.Map.ocaml = Set.eval_detailed t.ocaml ~has_native; melange }
    ;;

    let eval t ~has_native =
      eval_detailed t ~has_native |> Lib_mode.Map.map ~f:Option.is_some
    ;;
  end
end
