include Stdune
include Jbuilder_re
include Errors

(* To make bug reports usable *)
let () = Printexc.record_backtrace true

let sprintf = Printf.sprintf
let ksprintf = Printf.ksprintf

let initial_cwd = Sys.getcwd ()

module String_set = Set.Make(String)
module String_map = struct
  include Map.Make(String)

  let pp f fmt t =
    Format.pp_print_list (fun fmt (k, v) ->
      Format.fprintf fmt "@[<hov 2>(%s@ =@ %a)@]" k f v
    ) fmt (to_list t)
end

module Int_set = Set.Make(Int)
module Int_map = Map.Make(Int)

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

type ('a, 'b) eq = Eq : ('a, 'a) eq

type nothing = (int, string) eq

let protect  = Exn.protect
let protectx = Exn.protectx

let warn fmt =
  ksprintf (fun msg ->
    prerr_endline ("Warning: jbuild: " ^ msg))
    fmt

type fail = { fail : 'a. unit -> 'a }

let need_quoting s =
  let len = String.length s in
  len = 0 ||
  let rec loop i =
    if i = len then
      false
    else
      match s.[i] with
      | ' ' | '\"' -> true
      | _ -> loop (i + 1)
  in
  loop 0

let quote_for_shell s =
  if need_quoting s then
    Filename.quote s
  else
    s

let suggest_function : (string -> string list -> string list) ref = ref (fun _ _ -> [])

let hint name candidates =
  match !suggest_function name candidates with
  | [] -> ""
  | l ->
    let rec mk_hint = function
      | [a; b] -> sprintf "%s or %s" a b
      | [a] -> a
      | a :: l -> sprintf "%s, %s" a (mk_hint l)
      | [] -> ""
    in
    sprintf "\nHint: did you mean %s?" (mk_hint l)


(* [maybe_quoted s] is [s] if [s] doesn't need escaping according to OCaml lexing
   conventions and [sprintf "%S" s] otherwise. *)
let maybe_quoted s =
  let escaped = String.escaped s in
  if s == escaped || s = escaped then
    s
  else
    sprintf {|"%s"|} escaped

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

module Fmt = struct
  (* CR-someday diml: we should define a GADT for this:

     {[
       type 'a t =
         | Int : int t
         | Box : ...
         | Colored : ...
     ]}

     This way we could separate the creation of messages from the
     actual rendering.
  *)
  type 'a t = Format.formatter -> 'a -> unit

  let kstrf f fmt =
    let buf = Buffer.create 17 in
    let f fmt = Format.pp_print_flush fmt () ; f (Buffer.contents buf) in
    Format.kfprintf f (Format.formatter_of_buffer buf) fmt

  let failwith fmt = kstrf failwith fmt

  let list = Format.pp_print_list
  let string s ppf = Format.pp_print_string ppf s

  let nl = Format.pp_print_newline

  let prefix f g ppf x = f ppf; g ppf x
end

(* This is ugly *)
let printer = ref (Printf.eprintf "%s%!")
let print_to_console s = !printer s
