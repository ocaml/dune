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
  val open_tag  : t -> string
  val close_tag : t -> string
end

module Renderer = struct
  module type S = sig
    type tag

    val string
      :  unit
      -> (?margin:int -> tag t -> string) Staged.t
    val channel
      :  out_channel
      -> (?margin:int -> tag t -> unit) Staged.t
  end

  module Make(Tag : Tag_interpretation) = struct
    open Format

    external get16 : string -> int -> int         = "%caml_string_get16"
    external set16 : bytes  -> int -> int -> unit = "%caml_string_set16"

    let embed_tag tag =
      let open_tag  = Tag.open_tag  tag in
      let close_tag = Tag.close_tag tag in
      let open_len  = String.length open_tag  in
      let close_len = String.length close_tag in
      assert (open_len <= 0xffff);
      let buf = Bytes.create (2 + open_len + close_len) in
      set16 buf 0 open_len;
      Bytes.blit_string open_tag  0 buf  2   open_len;
      Bytes.blit_string close_tag 0 buf (2 + open_len) close_len;
      Bytes.unsafe_to_string buf

    let extract_opening_tag s =
      let open_len = get16 s 0 in
      String.sub s ~pos:2 ~len:open_len

    let extract_closing_tag s =
      let pos = 2 + get16 s 0 in
      String.sub s ~pos ~len:(String.length s - pos)

    let rec pp ppf t =
      match t with
      | Nop -> ()
      | Seq (a, b) -> pp ppf a; pp ppf b
      | Concat l -> List.iter l ~f:(pp ppf)
      | Vbox (indent, t) ->
        pp_open_vbox ppf indent;
        pp ppf t;
        pp_close_box ppf ()
      | Hbox t ->
        pp_open_hbox ppf ();
        pp ppf t;
        pp_close_box ppf ()
     | Int    x -> pp_print_int ppf x
     | String x -> pp_print_string ppf x
     | Char   x -> pp_print_char ppf x
     | List (f, l) -> pp_print_list (fun ppf x -> pp ppf (f x)) ppf l
     | Space -> pp_print_space ppf ()
     | Cut -> pp_print_cut ppf ()
     | Newline -> pp_force_newline ppf ()
     | Text s -> pp_print_text ppf s
     | Tag (tag, t) ->
       pp_open_tag ppf (embed_tag tag);
       pp ppf t;
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
      Staged.stage (fun ?(margin=80) t ->
        pp_set_margin ppf margin;
        pp ppf t;
        pp_print_flush ppf ();
        Buffer.contents buf)

    let channel oc =
      let ppf = formatter_of_out_channel oc in
      setup ppf;
      Staged.stage (fun ?(margin=80) t ->
        pp_set_margin ppf margin;
        pp ppf t;
        pp_print_flush ppf ())
  end
end

module Render = Renderer.Make(struct
    type t = unit
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
