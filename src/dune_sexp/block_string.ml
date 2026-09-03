open Import

module Kind = struct
  type t =
    | Escaped
    | Raw

  let repr =
    Repr.variant
      "dune-sexp-block-string-kind"
      [ Repr.case0 "Escaped" ~test:(function
          | Escaped -> true
          | Raw -> false)
      ; Repr.case0 "Raw" ~test:(function
          | Raw -> true
          | Escaped -> false)
      ]
  ;;
end

type t = (Kind.t * Template.Part.t list) list

let repr = Repr.list (Repr.pair Kind.repr (Repr.list Template.Part.repr))

let to_ast ~loc lines =
  let text = Buffer.create 16 in
  let parts = ref [] in
  let flush_text () =
    if Buffer.length text > 0
    then (
      parts := Template.Part.Text (Buffer.contents text) :: !parts;
      Buffer.clear text)
  in
  let add_part = function
    | Template.Part.Text string -> Buffer.add_string text string
    | Pform _ as pform ->
      flush_text ();
      parts := pform :: !parts
  in
  let rec collect first = function
    | [] -> ()
    | (_, line) :: lines ->
      if not first then Buffer.add_char text '\n';
      List.iter line ~f:add_part;
      collect false lines
  in
  collect true lines;
  match !parts with
  | [] -> Ast.Quoted_string (loc, Buffer.contents text)
  | _ ->
    flush_text ();
    Ast.Template { Template.quoted = true; parts = List.rev !parts; loc }
;;
