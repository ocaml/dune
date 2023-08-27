open Import

val term_size : (int * int) Lwd.var
val update_tabs : int -> Tabs.Tab.t -> unit

(** A backend that uses Notty to display the status line in the terminal. *)
val backend : unit -> Dune_console.Backend.t

module Widgets : sig
  module Button : module type of Button
  module Tabs : module type of Tabs
  module Scrollbox : module type of Scrollbox
end

module Drawing : module type of Drawing
module Import = Import
