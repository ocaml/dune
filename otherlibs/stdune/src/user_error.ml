exception E of User_message.t

let prefix = Pp.seq (Pp.tag User_message.Style.Error (Pp.verbatim "Error")) (Pp.char ':')

let make
      ?has_embedded_location
      ?needs_stack_trace
      ?loc
      ?hints
      ?compound
      ?promotion
      paragraphs
  =
  User_message.make
    ?has_embedded_location
    ?needs_stack_trace
    ?loc
    ?hints
    ?compound
    ?promotion
    paragraphs
    ~prefix
;;

let raise
      ?has_embedded_location
      ?needs_stack_trace
      ?loc
      ?hints
      ?compound
      ?promotion
      paragraphs
  =
  raise
    (E
       (make
          ?has_embedded_location
          ?needs_stack_trace
          ?loc
          ?hints
          ?compound
          ?promotion
          paragraphs))
;;

let ok_exn = function
  | Ok x -> x
  | Error msg -> Stdlib.raise (E msg)
;;

let reason =
  let reason = Pp.tag User_message.Style.Kwd (Pp.verbatim "Reason") in
  fun msg -> Pp.hovbox (Pp.concat [ reason; Pp.char ':'; Pp.space; msg ])
;;

let () =
  Printexc.register_printer (function
    | E t ->
      let open Pp.O in
      let pp =
        User_message.pp t
        ++
        match t.compound with
        | [] -> Pp.nop
        | compound -> Dyn.pp (Dyn.list User_message.Compound.to_dyn compound)
      in
      Some (Format.asprintf "%a" Pp.to_fmt pp)
    | _ -> None)
;;
