(** {1 Table manipulation}

    [Lwd_table] is an ordered collection with an impure interface.
    It is designed to be efficient in an interactive setting.

    The interface mimics the one of a doubly-linked lists: from a node, called
    row, you can iterate backward and forward, insert and delete other nodes,
    and change the value it is bound to.

    The sequence of nodes can be observed by map/reduce operations, that will
    be recomputed efficiently when sequence changes.
*)

type 'a t
type 'a row
(** The type of tables *)

val make : unit -> 'a t
(** Create a new table *)

(** {2 Inserting rows} *)

val prepend : ?set:'a -> 'a t -> 'a row
(** Insert and return a new row at the start of a table.
    It can be optionnally initialized to the value of [set]. *)

val append : ?set:'a -> 'a t -> 'a row
(** Insert and return a new row at the end of a table.
    It can be optionnally initialized to the value of [set]. *)

val prepend' : 'a t -> 'a -> unit
(* Insert a new initialized row at start of a table *)

val append' : 'a t -> 'a -> unit
(* Insert a new initialized row at end of a table *)

val before : ?set:'a -> 'a row -> 'a row
(** Insert and return a new row just before an existing row.
    It can be optionnally initialized to the value of [set].

    If the input row is unbound ([is_bound] returns false), the returned row is
    too.
*)

val after : ?set:'a -> 'a row -> 'a row
(** Insert and return a new row just after an existing row.
    It can be optionnally initialized to the value of [set].

    If the input row is unbound ([is_bound] returns false), the returned row is
    too.
*)

(** {2 Iterating over rows} *)

val first : 'a t -> 'a row option
(** Returns the first row of a table, or [None] if the table is empty *)

val last : 'a t -> 'a row option
(** Returns the last row of a table, or [None] if the table is empty *)

val next : 'a row -> 'a row option
(** Returns the row next to another one, or [None] if the input row is unbound
   or is the last row *)

val prev : 'a row -> 'a row option
(** Returns the row just before another one, or [None] if the input row is
   unbound or is the first row *)

(** {2 Accessing and changing row contents} *)

val get : 'a row -> 'a option
(** Get the value associated with a row, if any, or [None] if the row is
    unbound *)

val set : 'a row -> 'a -> unit
(** Set the value associated with a row, or do nothing if the row is unbound *)

val unset : 'a row -> unit
(** Unset the value associated with a row *)

(** {2 Removing rows} *)

val is_bound : 'a row -> bool
(** Returns [true] iff the row is bound in a table (it has not beem [remove]d
    yet, the table has not been [clear]ed) *)

val remove : 'a row -> unit
(** [remove] a row from its table, [is_bound] will be [true] after that *)

val clear : 'a t -> unit
(** Remove all rows from a table *)

(** {2 Observing table contents} *)

val reduce : 'a Lwd_utils.monoid -> 'a t -> 'a Lwd.t
(** Observe the content of a table by reducing it with a monoid *)

val map_reduce : ('a row -> 'a -> 'b) -> 'b Lwd_utils.monoid -> 'a t -> 'b Lwd.t
(** Observe the content of a table by mapping and reducing it *)

val iter : ('a -> unit) -> 'a t -> unit
(** Immediate, non reactive, iteration over elements of a table *)
