let all ?(pos = 0) ?len re s : _ Seq.t =
  if pos < 0 then invalid_arg "Re.all";
  (* index of the first position we do not consider.
     !pos < limit is an invariant *)
  let limit =
    match len with
    | None -> String.length s
    | Some l ->
      if l < 0 || pos + l > String.length s then invalid_arg "Re.all";
      pos + l
  in
  (* iterate on matches. When a match is found, search for the next
     one just after its end *)
  let rec aux pos on_match () =
    if pos > limit
    then Seq.Nil (* no more matches *)
    else (
      match
        Compile.match_str ~groups:true ~partial:false re s ~pos ~len:(limit - pos)
      with
      | Match substr ->
        let p1, p2 = Group.offset substr 0 in
        if on_match && p1 = pos && p1 = p2
        then (* skip empty match right after a match *)
          aux (pos + 1) false ()
        else (
          let pos = if p1 = p2 then p2 + 1 else p2 in
          Seq.Cons (substr, aux pos (p1 <> p2)))
      | Running _ | Failed -> Seq.Nil)
  in
  aux pos false
;;

let matches ?pos ?len re s : _ Seq.t =
  all ?pos ?len re s |> Seq.map (fun sub -> Group.get sub 0)
;;

let split_full ?(pos = 0) ?len re s : _ Seq.t =
  if pos < 0 then invalid_arg "Re.split";
  let limit =
    match len with
    | None -> String.length s
    | Some l ->
      if l < 0 || pos + l > String.length s then invalid_arg "Re.split";
      pos + l
  in
  (* i: start of delimited string
     pos: first position after last match of [re]
     limit: first index we ignore (!pos < limit is an invariant) *)
  let pos0 = pos in
  let rec aux state i pos () =
    match state with
    | `Idle when pos > limit ->
      (* We had an empty match at the end of the string *)
      assert (i = limit);
      Seq.Nil
    | `Idle ->
      (match
         Compile.match_str ~groups:true ~partial:false re s ~pos ~len:(limit - pos)
       with
       | Match substr ->
         let p1, p2 = Group.offset substr 0 in
         let pos = if p1 = p2 then p2 + 1 else p2 in
         let old_i = i in
         let i = p2 in
         if old_i = p1 && p1 = p2 && p1 > pos0
         then (* Skip empty match right after a delimiter *)
           aux state i pos ()
         else if p1 > pos0
         then (
           (* string does not start by a delimiter *)
           let text = String.sub s old_i (p1 - old_i) in
           let state = `Yield (`Delim substr) in
           Seq.Cons (`Text text, aux state i pos))
         else Seq.Cons (`Delim substr, aux state i pos)
       | Running _ -> Seq.Nil
       | Failed ->
         if i < limit
         then (
           let text = String.sub s i (limit - i) in
           (* yield last string *)
           Seq.Cons (`Text text, aux state limit pos))
         else Seq.Nil)
    | `Yield x -> Seq.Cons (x, aux `Idle i pos)
  in
  aux `Idle pos pos
;;

let split ?pos ?len re s : _ Seq.t =
  let seq = split_full ?pos ?len re s in
  let rec filter seq () =
    match seq () with
    | Seq.Nil -> Seq.Nil
    | Seq.Cons (`Delim _, tl) -> filter tl ()
    | Seq.Cons (`Text s, tl) -> Seq.Cons (s, filter tl)
  in
  filter seq
;;

let split_delim ?pos ?len re s : _ Seq.t =
  let seq = split_full ?pos ?len re s in
  let rec filter ~delim seq () =
    match seq () with
    | Seq.Nil -> if delim then Seq.Cons ("", fun () -> Seq.Nil) else Seq.Nil
    | Seq.Cons (`Delim _, tl) ->
      if delim
      then Seq.Cons ("", fun () -> filter ~delim:true tl ())
      else filter ~delim:true tl ()
    | Seq.Cons (`Text s, tl) -> Seq.Cons (s, filter ~delim:false tl)
  in
  filter ~delim:true seq
;;
