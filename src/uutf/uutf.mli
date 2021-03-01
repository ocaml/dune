(** UTF-8 functions *)

(** The API of this library is similar to a subset of the API of the
    https://github.com/dbuenzli/uutf. *)

module String : sig
  val fold_utf_8 :
       ('a -> int -> [ `Uchar of Uchar.t | `Malformed of string ] -> 'a)
    -> 'a
    -> string
    -> 'a
end

module Buffer : sig
  val add_utf_8 : Buffer.t -> Uchar.t -> unit
end
