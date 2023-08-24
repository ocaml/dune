include Pp

let compare ~compare x y =
  Ordering.of_int (Pp.compare (fun a b -> Ordering.to_int (compare a b)) x y)
;;

let to_dyn tag_to_dyn t =
  match Pp.to_ast t with
  | Error _ -> Dyn.variant "Contains Format" [ Dyn.opaque "<error>" ]
  | Ok t ->
    let open Dyn in
    let rec to_dyn t =
      match (t : _ Pp.Ast.t) with
      | Nop -> variant "Nop" []
      | Seq (x, y) -> variant "Seq" [ to_dyn x; to_dyn y ]
      | Concat (x, y) -> variant "Concat" [ to_dyn x; list to_dyn y ]
      | Box (i, t) -> variant "Box" [ int i; to_dyn t ]
      | Vbox (i, t) -> variant "Vbox" [ int i; to_dyn t ]
      | Hbox t -> variant "Hbox" [ to_dyn t ]
      | Hvbox (i, t) -> variant "Hvbox" [ int i; to_dyn t ]
      | Hovbox (i, t) -> variant "Hovbox" [ int i; to_dyn t ]
      | Verbatim s -> variant "Verbatim" [ string s ]
      | Char c -> variant "Char" [ char c ]
      | Break (x, y) ->
        variant "Break" [ triple string int string x; triple string int string y ]
      | Newline -> variant "Newline" []
      | Text s -> variant "Text" [ string s ]
      | Tag (s, t) -> variant "Tag" [ tag_to_dyn s; to_dyn t ]
    in
    to_dyn t
;;

let truncate length pp =
  let ast =
    match Pp.to_ast pp with
    | Ok ast -> ast
    | Error _ -> assert false
  in
  let rec loop length_so_far pp : int * _ Pp.Ast.t =
    if length_so_far >= length
    then 0, Nop
    else (
      match (pp : _ Pp.Ast.t) with
      | Nop | Newline | Break _ -> 0, Nop
      | Seq (pp1, pp2) ->
        let length_of_pp1, pp1 = loop length_so_far pp1 in
        let length_of_pp2, pp2 = loop (length_so_far + length_of_pp1) pp2 in
        length_of_pp1 + length_of_pp2, Seq (pp1, pp2)
      | Concat (pp1, pps) ->
        let length_of_pp1, pp1 = loop length_so_far pp1 in
        let length_of_pps, pps =
          List.fold_map
            pps
            ~init:(length_so_far + length_of_pp1)
            ~f:(fun length_so_far pp ->
              let length, pp = loop length_so_far pp in
              length_so_far + length, pp)
        in
        length_of_pp1 + length_of_pps, Concat (pp1, pps)
      (* Horiztonal boxes are preserved. Hovbox, Hvbox and Box are turned into Hbox since
         that is the only sensible format box we can work with. *)
      | Hbox pp | Hovbox (_, pp) | Hvbox (_, pp) | Box (_, pp) | Vbox (_, pp) ->
        let length, pp = loop length_so_far pp in
        length, Hbox pp
      (* Tags are preserved. *)
      | Tag (tag, pp) ->
        let length, pp = loop length_so_far pp in
        length, Tag (tag, pp)
      | Verbatim s ->
        let length_of_s = String.length s in
        if length_so_far + length_of_s > length
        then
          ( length - length_so_far
          , Verbatim (String.sub s ~pos:0 ~len:(length - length_so_far)) )
        else length_of_s, Verbatim s
      | Text s ->
        let length_of_s = String.length s in
        if length_so_far + length_of_s > length
        then
          length - length_so_far, Text (String.sub s ~pos:0 ~len:(length - length_so_far))
        else length_of_s, Text s
      | Char c -> if length_so_far + 1 > length then 0, Nop else 1, Char c)
  in
  let truncated_length, pp = loop 0 ast in
  assert (truncated_length <= length);
  let pp =
    if truncated_length < length
    then pp
    else (
      let _, pp = loop 3 pp in
      Pp.Ast.Seq (pp, Text "..."))
  in
  Pp.of_ast pp
;;
