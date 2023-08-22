module List = struct
  include ListLabels

  let map ~f t = rev (rev_map ~f t)
end

module String = StringLabels

module Ast = struct
  type +'a t =
    | Nop
    | Seq of 'a t * 'a t
    | Concat of 'a t * 'a t list
    | Box of int * 'a t
    | Vbox of int * 'a t
    | Hbox of 'a t
    | Hvbox of int * 'a t
    | Hovbox of int * 'a t
    | Verbatim of string
    | Char of char
    | Break of (string * int * string) * (string * int * string)
    | Newline
    | Text of string
    | Tag of 'a * 'a t
end

type +'a t =
  | Nop
  | Seq of 'a t * 'a t
  | Concat of 'a t * 'a t list
  | Box of int * 'a t
  | Vbox of int * 'a t
  | Hbox of 'a t
  | Hvbox of int * 'a t
  | Hovbox of int * 'a t
  | Verbatim of string
  | Char of char
  | Break of (string * int * string) * (string * int * string)
  | Newline
  | Text of string
  | Tag of 'a * 'a t
  | Format of (Format.formatter -> unit)

let rec of_ast : 'a. 'a Ast.t -> 'a t = function
  | Nop -> Nop
  | Seq (x, y) -> Seq (of_ast x, of_ast y)
  | Concat (x, y) -> Concat (of_ast x, List.map ~f:of_ast y)
  | Box (x, y) -> Box (x, of_ast y)
  | Vbox (x, y) -> Vbox (x, of_ast y)
  | Hbox x -> Hbox (of_ast x)
  | Hvbox (x, y) -> Hvbox (x, of_ast y)
  | Hovbox (x, y) -> Hovbox (x, of_ast y)
  | Verbatim s -> Verbatim s
  | Char c -> Char c
  | Break (x, y) -> Break (x, y)
  | Newline -> Newline
  | Text s -> Text s
  | Tag (a, x) -> Tag (a, of_ast x)

let to_ast x =
  let rec to_ast : 'a t -> 'a Ast.t = function
    | Nop -> Nop
    | Seq (x, y) -> Seq (to_ast x, to_ast y)
    | Concat (x, y) -> Concat (to_ast x, List.map ~f:(fun x -> to_ast x) y)
    | Box (x, y) -> Box (x, to_ast y)
    | Vbox (x, y) -> Vbox (x, to_ast y)
    | Hbox x -> Hbox (to_ast x)
    | Hvbox (x, y) -> Hvbox (x, to_ast y)
    | Hovbox (x, y) -> Hovbox (x, to_ast y)
    | Verbatim s -> Verbatim s
    | Char c -> Char c
    | Break (x, y) -> Break (x, y)
    | Newline -> Newline
    | Tag (a, x) -> Tag (a, to_ast x)
    | Text s -> Text s
    | Format _ -> raise_notrace Exit
  in
  try Ok (to_ast x) with
  | Exit -> Error ()

let rec map_tags t ~f =
  match t with
  | Nop -> Nop
  | Seq (a, b) -> Seq (map_tags a ~f, map_tags b ~f)
  | Concat (sep, l) -> Concat (map_tags sep ~f, List.map l ~f:(map_tags ~f))
  | Box (indent, t) -> Box (indent, map_tags t ~f)
  | Vbox (indent, t) -> Vbox (indent, map_tags t ~f)
  | Hbox t -> Hbox (map_tags t ~f)
  | Hvbox (indent, t) -> Hvbox (indent, map_tags t ~f)
  | Hovbox (indent, t) -> Hovbox (indent, map_tags t ~f)
  | (Verbatim _ | Char _ | Break _ | Newline | Text _) as t -> t
  | Tag (tag, t) -> Tag (f tag, map_tags t ~f)
  | Format f -> Format f

let rec filter_map_tags t ~f =
  match t with
  | Nop -> Nop
  | Seq (a, b) -> Seq (filter_map_tags a ~f, filter_map_tags b ~f)
  | Concat (sep, l) ->
    Concat (filter_map_tags sep ~f, List.map l ~f:(filter_map_tags ~f))
  | Box (indent, t) -> Box (indent, filter_map_tags t ~f)
  | Vbox (indent, t) -> Vbox (indent, filter_map_tags t ~f)
  | Hbox t -> Hbox (filter_map_tags t ~f)
  | Hvbox (indent, t) -> Hvbox (indent, filter_map_tags t ~f)
  | Hovbox (indent, t) -> Hovbox (indent, filter_map_tags t ~f)
  | (Verbatim _ | Char _ | Break _ | Newline | Text _) as t -> t
  | Tag (tag, t) -> (
    let t = filter_map_tags t ~f in
    match f tag with
    | None -> t
    | Some tag -> Tag (tag, t))
  | Format f -> Format f

module Render = struct
  open Format

  let rec render ppf t ~tag_handler =
    match t with
    | Nop -> ()
    | Seq (a, b) ->
      render ppf ~tag_handler a;
      render ppf ~tag_handler b
    | Concat (_, []) -> ()
    | Concat (sep, x :: l) ->
      render ppf ~tag_handler x;
      List.iter l ~f:(fun x ->
          render ppf ~tag_handler sep;
          render ppf ~tag_handler x)
    | Box (indent, t) ->
      pp_open_box ppf indent;
      render ppf ~tag_handler t;
      pp_close_box ppf ()
    | Vbox (indent, t) ->
      pp_open_vbox ppf indent;
      render ppf ~tag_handler t;
      pp_close_box ppf ()
    | Hbox t ->
      pp_open_hbox ppf ();
      render ppf ~tag_handler t;
      pp_close_box ppf ()
    | Hvbox (indent, t) ->
      pp_open_hvbox ppf indent;
      render ppf ~tag_handler t;
      pp_close_box ppf ()
    | Hovbox (indent, t) ->
      pp_open_hovbox ppf indent;
      render ppf ~tag_handler t;
      pp_close_box ppf ()
    | Verbatim x -> pp_print_string ppf x
    | Char x -> pp_print_char ppf x
    | Break (fits, breaks) -> pp_print_custom_break ppf ~fits ~breaks
    | Newline -> pp_force_newline ppf ()
    | Text s -> pp_print_text ppf s
    | Tag (tag, t) -> tag_handler ppf tag t
    | Format f -> f ppf
end

let to_fmt_with_tags = Render.render

let rec to_fmt ppf t =
  Render.render ppf t ~tag_handler:(fun ppf _tag t -> to_fmt ppf t)

let nop = Nop
let seq a b = Seq (a, b)

let concat ?(sep = Nop) = function
  | [] -> Nop
  | [ x ] -> x
  | l -> Concat (sep, l)

let concat_map ?(sep = Nop) l ~f =
  match l with
  | [] -> Nop
  | [ x ] -> f x
  | l -> Concat (sep, List.map l ~f)

let concat_mapi ?(sep = Nop) l ~f =
  match l with
  | [] -> Nop
  | [ x ] -> f 0 x
  | l -> Concat (sep, List.mapi l ~f)

let box ?(indent = 0) t = Box (indent, t)
let vbox ?(indent = 0) t = Vbox (indent, t)
let hbox t = Hbox t
let hvbox ?(indent = 0) t = Hvbox (indent, t)
let hovbox ?(indent = 0) t = Hovbox (indent, t)
let verbatim x = Verbatim x
let char x = Char x
let custom_break ~fits ~breaks = Break (fits, breaks)

let break ~nspaces ~shift =
  custom_break ~fits:("", nspaces, "") ~breaks:("", shift, "")

let space = break ~nspaces:1 ~shift:0
let cut = break ~nspaces:0 ~shift:0
let newline = Newline
let text s = Text s
let textf fmt = Printf.ksprintf text fmt
let tag tag t = Tag (tag, t)

let enumerate l ~f =
  vbox
    (concat ~sep:cut
       (List.map l ~f:(fun x -> box ~indent:2 (seq (verbatim "- ") (f x)))))

let chain l ~f =
  vbox
    (concat ~sep:cut
       (List.mapi l ~f:(fun i x ->
            box ~indent:3
              (seq
                 (verbatim
                    (if i = 0 then
                       "   "
                     else
                       "-> "))
                 (f x)))))

module O = struct
  let ( ++ ) = seq
end

let of_fmt f x = Format (fun ppf -> f ppf x)

let compare =
  let compare_both (type a b) (f : a -> a -> int) (g : b -> b -> int) (a, b)
      (c, d) =
    let r = f a c in
    if r <> 0 then
      r
    else
      g b d
  in
  (* Due to 4.08 lower bound, we need to define this here. *)
  let rec compare_list a b ~cmp:f : int =
    match (a, b) with
    | [], [] -> 0
    | [], _ :: _ -> -1
    | _ :: _, [] -> 1
    | x :: a, y :: b -> (
      match (f x y : int) with
      | 0 -> compare_list a b ~cmp:f
      | ne -> ne)
  in
  fun compare_tag ->
    let rec compare x y =
      match (x, y) with
      | Nop, Nop -> 0
      | Nop, _ -> -1
      | _, Nop -> 1
      | Seq (a, b), Seq (c, d) -> compare_both compare compare (a, b) (c, d)
      | Seq _, _ -> -1
      | _, Seq _ -> 1
      | Concat (a, b), Concat (c, d) ->
        compare_both compare (compare_list ~cmp:compare) (a, b) (c, d)
      | Concat _, _ -> -1
      | _, Concat _ -> 1
      | Box (a, b), Box (c, d) -> compare_both Int.compare compare (a, b) (c, d)
      | Box _, _ -> -1
      | _, Box _ -> 1
      | Vbox (a, b), Vbox (c, d) ->
        compare_both Int.compare compare (a, b) (c, d)
      | Vbox _, _ -> -1
      | _, Vbox _ -> 1
      | Hbox a, Hbox b -> compare a b
      | Hbox _, _ -> -1
      | _, Hbox _ -> 1
      | Hvbox (a, b), Hvbox (c, d) ->
        compare_both Int.compare compare (a, b) (c, d)
      | Hvbox _, _ -> -1
      | _, Hvbox _ -> 1
      | Hovbox (a, b), Hovbox (c, d) ->
        compare_both Int.compare compare (a, b) (c, d)
      | Hovbox _, _ -> -1
      | _, Hovbox _ -> 1
      | Verbatim a, Verbatim b -> String.compare a b
      | Verbatim _, _ -> -1
      | _, Verbatim _ -> 1
      | Char a, Char b -> Char.compare a b
      | Char _, _ -> -1
      | _, Char _ -> 1
      | Break (a, b), Break (c, d) ->
        let compare (x, y, z) (a, b, c) =
          compare_both String.compare
            (compare_both Int.compare String.compare)
            (x, (y, z))
            (a, (b, c))
        in
        compare_both compare compare (a, b) (c, d)
      | Break _, _ -> -1
      | _, Break _ -> 1
      | Newline, Newline -> 0
      | Newline, _ -> -1
      | _, Newline -> 1
      | Text a, Text b -> String.compare a b
      | Text _, _ -> -1
      | _, Text _ -> 1
      | Tag (a, b), Tag (c, d) -> compare_both compare_tag compare (a, b) (c, d)
      | Format _, Format _ ->
        raise
          (Invalid_argument "[Pp.of_fmt] values not supported in [Pp.compare]")
      | Format _, _ -> -1
      | _, Format _ -> 1
    in
    compare
