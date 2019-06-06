module List = Dune_caml.ListLabels
module String = Dune_caml.StringLabels

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
  | Break of int * int
  | Newline
  | Text of string
  | Tag of 'a * 'a t

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
  | Verbatim _ | Char _ | Break _ | Newline | Text _ -> t
  | Tag (tag, t) -> Tag (f tag, map_tags t ~f)

module type Tag = sig
  type t
  module Handler : sig
    type tag = t
    type t
    val init : t
    val handle : t -> tag -> string * t * string
  end with type tag := t
end

module Renderer = struct
  module type S = sig
    module Tag : Tag

    val string
      :  unit
      -> (?margin:int -> ?tag_handler:Tag.Handler.t -> Tag.t t -> string)
           Staged.t
    val channel
      :  out_channel
      -> (?margin:int -> ?tag_handler:Tag.Handler.t -> Tag.t t -> unit)
           Staged.t
  end

  module Make(Tag : Tag) = struct
    open Format

    module Tag = Tag

    (* The format interface only support string for tags, so we embed
       then as follow:

       - length of opening string on 16 bits
       - opening string
       - closing string
    *)
    external get16 : string -> int -> int         = "%caml_string_get16"
    external set16 : bytes  -> int -> int -> unit = "%caml_string_set16"

    let embed_tag ~opening ~closing =
      let opening_len = String.length opening  in
      let closing_len = String.length closing in
      assert (opening_len <= 0xffff);
      let buf = Bytes.create (2 + opening_len + closing_len) in
      set16 buf 0 opening_len;
      Bytes.blit_string ~src:opening ~src_pos:0 ~dst:buf ~dst_pos:2   ~len:opening_len;
      Bytes.blit_string ~src:closing ~src_pos:0 ~dst:buf ~dst_pos:(2 + opening_len) ~len:closing_len;
      Bytes.unsafe_to_string buf

    let extract_opening_tag s =
      let open_len = get16 s 0 in
      String.sub s ~pos:2 ~len:open_len

    let extract_closing_tag s =
      let pos = 2 + get16 s 0 in
      String.sub s ~pos ~len:(String.length s - pos)

    let rec pp th ppf t =
      match t with
      | Nop -> ()
      | Seq (a, b) -> pp th ppf a; pp th ppf b
      | Concat (_, []) -> ()
      | Concat (sep, x :: l) ->
        pp th ppf x;
        List.iter l ~f:(fun x ->
          pp th ppf sep;
          pp th ppf x)
      | Box (indent, t) ->
        pp_open_box ppf indent;
        pp th ppf t;
        pp_close_box ppf ()
      | Vbox (indent, t) ->
        pp_open_vbox ppf indent;
        pp th ppf t;
        pp_close_box ppf ()
      | Hbox t ->
        pp_open_hbox ppf ();
        pp th ppf t;
        pp_close_box ppf ()
      | Hvbox (indent, t) ->
        pp_open_hvbox ppf indent;
        pp th ppf t;
        pp_close_box ppf ()
      | Hovbox (indent, t) ->
        pp_open_hovbox ppf indent;
        pp th ppf t;
        pp_close_box ppf ()
      | Verbatim x -> pp_print_string ppf x
      | Char   x -> pp_print_char ppf x
      | Break (nspaces, shift) -> pp_print_break ppf nspaces shift
      | Newline -> pp_force_newline ppf ()
      | Text s -> pp_print_text ppf s
      | Tag (tag, t) ->
        let opening, th, closing = Tag.Handler.handle th tag in
        pp_open_tag ppf (embed_tag ~opening ~closing);
        pp th ppf t;
        pp_close_tag ppf ()

    let setup ppf =
      let funcs = pp_get_formatter_tag_functions ppf () in
      pp_set_mark_tags ppf true;
      pp_set_formatter_tag_functions ppf
        { funcs with
          mark_open_tag  = extract_opening_tag
        ; mark_close_tag = extract_closing_tag
        }

    let string () =
      let buf = Buffer.create 1024 in
      let ppf = formatter_of_buffer buf in
      setup ppf;
      Staged.stage (fun ?(margin=80) ?(tag_handler=Tag.Handler.init) t ->
        pp_set_margin ppf margin;
        pp tag_handler ppf t;
        pp_print_flush ppf ();
        let s = Buffer.contents buf in
        Buffer.clear buf;
        s)

    let channel oc =
      let ppf = formatter_of_out_channel oc in
      setup ppf;
      Staged.stage (fun ?(margin=80) ?(tag_handler=Tag.Handler.init) t ->
        pp_set_margin ppf margin;
        pp tag_handler ppf t;
        pp_print_flush ppf ())
  end
end

module Render = Renderer.Make(struct
    type t = unit
    module Handler = struct
      type t   = unit
      let init = ()
      let handle () () = "", (), ""
    end
  end)

let pp ppf t = Render.pp () ppf t

let nop = Nop
let seq a b = Seq (a, b)
let concat ?(sep=Nop) = function
  | [] -> Nop
  | [x] -> x
  | l -> Concat (sep, l)
let concat_map ?(sep=Nop) l ~f =
  match l with
  | [] -> Nop
  | [x] -> f x
  | l -> Concat (sep, List.map l ~f)
let box ?(indent=0) l = Box (indent, concat l)
let vbox ?(indent=0) l = Vbox (indent, concat l)
let hbox l = Hbox (concat l)
let hvbox ?(indent=0) l = Hvbox (indent, concat l)
let hovbox ?(indent=0) l = Hovbox (indent, concat l)

let verbatim x = Verbatim x
let char x = Char x

let break ~nspaces ~shift = Break (nspaces, shift)
let space = Break (1, 0)
let cut = Break (0, 0)
let newline = Newline

let text s = Text s
let textf fmt = Printf.ksprintf text fmt

let tag t ~tag = Tag (tag, t)

let enumerate l ~f =
  vbox [ concat ~sep:cut (List.map l ~f:(fun x ->
    box ~indent:2
      [ seq (char '-') (seq space (f x)) ])
  ]
