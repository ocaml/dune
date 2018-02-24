module List = ListLabels

type +'a t =
  | Nop
  | Seq of 'a t * 'a t
  | Concat of 'a t list
  | Vbox of int * 'a t
  | Hbox of 'a t
  | Int of int
  | String of string
  | Char of char
  | List : ('a -> 'b t) * 'a list -> 'b t
  | Space
  | Cut
  | Newline
  | Text of string
  | Tag of 'a * 'a t

module type Tag_interpretation = sig
  type t
  val compare : t -> t -> Ordering.t
  val open_tag  : t -> string
  val close_tag : t -> string
end

module Smap = Map.Make(String)

module Renderer = struct
  module type S = sig
    type tag

    val to_string  : ?margin:int -> tag t -> string
    val to_channel : ?margin:int -> tag t -> out_channel -> unit
  end

  module Make(Tag : Tag_interpretation) = struct
    open Format

    module Tmap = Map.Make(Tag)

    type tags =
      { mutable next : int
      ; mutable tag2str : string Tmap.t
      ; mutable str2tag : Tag.t Smap.t
      }

    let string_of_tag tags tag =
      match Tmap.find tags.tag2str tag with
      | Some s -> s
      | None ->
        let key = string_of_int tags.next in
        tags.next <- tags.next + 1;
        tags.tag2str <- Tmap.add tags.tag2str tag key;
        tags.str2tag <- Smap.add tags.str2tag key tag;
        key

    let tag_of_string tags key =
      Option.value_exn (Smap.find tags.str2tag key)

    let rec pp tags ppf t =
      match t with
      | Nop -> ()
      | Seq (a, b) -> pp tags ppf a; pp tags ppf b
      | Concat l -> List.iter l ~f:(pp tags ppf)
      | Vbox (indent, t) ->
        pp_open_vbox ppf indent;
        pp tags ppf t;
        pp_close_box ppf ()
      | Hbox t ->
        pp_open_hbox ppf ();
        pp tags ppf t;
        pp_close_box ppf ()
     | Int    x -> pp_print_int ppf x
     | String x -> pp_print_string ppf x
     | Char   x -> pp_print_char ppf x
     | List (f, l) -> pp_print_list (fun ppf x -> pp tags ppf (f x)) ppf l
     | Space -> pp_print_space ppf ()
     | Cut -> pp_print_cut ppf ()
     | Newline -> pp_force_newline ppf ()
     | Text s -> pp_print_text ppf s
     | Tag (tag, t) ->
       let s = string_of_tag tags tag in
       pp_open_tag ppf s;
       pp tags ppf t;
       pp_close_tag ppf ()

    let pp ppf ?(margin=80) t =
      pp_set_margin ppf margin;
      let tags =
        { next = 0
        ; tag2str = Tmap.empty
        ; str2tag = Smap.empty
        }
      in
      let funcs = pp_get_formatter_tag_functions ppf () in
      pp_set_mark_tags ppf true;
      pp_set_formatter_tag_functions ppf
        { funcs with
          mark_close_tag = (fun tag -> Tag.close_tag (tag_of_string tags tag))
        ; mark_open_tag  = (fun tag -> Tag. open_tag (tag_of_string tags tag))
        };
      pp tags ppf t;
      pp_print_flush ppf ()

    let to_string ?margin t =
      let buf = Buffer.create 256 in
      let ppf = formatter_of_buffer buf in
      pp ppf t ?margin;
      Buffer.contents buf

    let to_channel ?margin t oc =
      let ppf = formatter_of_out_channel oc in
      pp ppf t ?margin
  end
end

module Render = Renderer.Make(struct
    type t = unit
    let compare () () = Ordering.Eq
    let open_tag  () = ""
    let close_tag () = ""
  end)

let nop = Nop
let seq a b = Seq (a, b)
let concat l = Concat l
let vbox ?(indent=0) t = Vbox (indent, t)
let hbox t = Hbox t

let int x = Int x
let string x = String x
let char x = Char x
let list f l = List (f, l)

let space = Space
let cut = Cut
let newline = Newline

let text s = Text s

let tag tag t = Tag (tag, t)
