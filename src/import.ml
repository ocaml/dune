open! Stdune

include Stdune

module Re = Dune_re

(* To make bug reports usable *)
let () = Printexc.record_backtrace true

let sprintf = Printf.sprintf
let ksprintf = Printf.ksprintf

let initial_cwd = Sys.getcwd ()

module Sys = struct
  include Sys

  let force_remove =
    if win32 then
      fun fn ->
        try
          remove fn
        with Sys_error _ ->
          (* Try to remove the "read-only" attribute, then retry. *)
          (try Unix.chmod fn 0o666 with Unix.Unix_error _ -> ());
          remove fn
    else
      remove
end

let protect  = Exn.protect
let protectx = Exn.protectx

type fail = { fail : 'a. unit -> 'a }

let need_quoting s =
  let len = String.length s in
  len = 0 ||
  let rec loop i =
    if i = len then
      false
    else
      match s.[i] with
      | ' ' | '\"' | '(' | ')' | '{' | '}' | ';' | '#' -> true
      | _ -> loop (i + 1)
  in
  loop 0

let quote_for_shell s =
  if need_quoting s then
    Filename.quote s
  else
    s

(* Disable file operations to force to use the IO module *)
let open_in      = `Use_Io
let open_in_bin  = `Use_Io
let open_in_gen  = `Use_Io
let open_out     = `Use_Io
let open_out_bin = `Use_Io
let open_out_gen = `Use_Io

(* We open this module at the top of module generating rules, to make sure they don't do
   Io manually *)
module No_io = struct
  module Io = struct end
end

let print_to_console = Console.print

let pp_cycle cycle ~f =
  Pp.vbox [
    Pp.concat ~sep:Pp.cut (List.mapi l ~f:(fun i x ->
      Pp.box ~indent:3
        [ Pp.seq (if i = 0 then
                    Pp.(seq space space)
                  else
                    Pp.verbatim "->")
            (Pp.seq Pp.space (f x))
        ]))
  ]
