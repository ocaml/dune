(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2018 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamParserTypes.FullPos
open OpamTypesBase
open OpamStd.Op

type bad_format = pos option * string

exception Bad_version of bad_format
exception Bad_format of bad_format
exception Bad_format_list of bad_format list

let bad_format ?pos fmt =
  Printf.ksprintf
    (fun str ->
       raise (Bad_format (pos,str)))
    fmt

let bad_version ?pos fmt =
  Printf.ksprintf
    (fun str ->
       raise (Bad_version (pos,str)))
    fmt

let add_pos pos = function
  | Bad_format (pos_opt,msg) as e ->
    if pos_opt = None || pos_opt = Some pos_null
    then Bad_format (Some pos, msg)
    else e
  | Bad_version (pos_opt,msg) as e ->
    if pos_opt = None || pos_opt = Some pos_null
    then Bad_version (Some pos, msg)
    else e

  | e -> e

let rec string_of_bad_format ?file e =
  match e, file with
  | Bad_version (None, msg), Some filename
  | Bad_format (None, msg), Some filename
  | Bad_version (Some {filename; start = -1, -1 ; stop = -1,-1 }, msg), _
  | Bad_format (Some {filename; start = -1, -1 ; stop = -1,-1 }, msg), _ ->
    Printf.sprintf "In %s:\n%s" filename msg
  | Bad_version (Some pos, msg), _
  | Bad_format (Some pos, msg), _ ->
    Printf.sprintf "At %s:\n%s" (string_of_pos pos) msg
  | Bad_version (None, msg), None
  | Bad_format (None, msg), None ->
    Printf.sprintf "Input error:\n%s" msg
  | Bad_format_list bfl, _ ->
    OpamStd.List.concat_map "\n"
      (fun bf -> string_of_bad_format ?file (Bad_format bf)) bfl
  | _ -> Printexc.to_string e

let () = Printexc.register_printer @@ function
  | (Bad_version _ | Bad_format _ | Bad_format_list _ as e) ->
    Some (string_of_bad_format ?file:None e)
  | _ -> None

type ('a,'b) t = {
  parse: pos:pos -> 'a -> 'b;
  print: 'b -> 'a;
  ppname: string;
  name_constr: string -> string;
}

let pp ?(name="") ?(name_constr=fun x -> x) parse print =
  {
    parse; print; ppname = name; name_constr;
  }

let of_pair name (simple_parse, print) =
  pp ~name (fun ~pos:_ -> simple_parse) print

(** Utility functions *)

exception Unexpected of pos option
let unexpected ?pos () = raise (Unexpected pos)

(** Basic pp usage *)

let parse pp ~pos x = try pp.parse ~pos x with
  | Bad_version _ | Bad_format _ | Bad_format_list _ as e ->
    raise (add_pos pos e)
  | Unexpected (Some pos) -> bad_format ~pos "expected %s" pp.ppname
  | Unexpected None -> bad_format ~pos "expected %s" pp.ppname
  | Failure msg ->
    bad_format ~pos "%s%s"
      (if pp.ppname <> "" then Printf.sprintf "while expecting %s: " pp.ppname
       else "")
      msg
  | e ->
    OpamStd.Exn.fatal e;
    bad_format ~pos "%s%s"
      (if pp.ppname <> "" then Printf.sprintf "while expecting %s: " pp.ppname
       else "")
      (Printexc.to_string e)

let print pp x = pp.print x


(** Pp combination and transformation *)

(** Piping *)
let (-|) pp1 pp2 = {
  parse = (fun ~pos x ->
      let y = pp1.parse ~pos x in
      parse pp2 ~pos y
    );
  print = pp1.print @* pp2.print;
  ppname =
    (match pp2.ppname with "" -> pp1.ppname
                         | name -> pp1.name_constr name);
  name_constr = pp1.name_constr @* pp2.name_constr;
}

let identity = {
  parse = (fun ~pos:_ x -> x);
  print = (fun x -> x);
  ppname = "";
  name_constr = (fun x -> x);
}

let ignore = {
  parse = (fun ~pos:_ -> OpamStd.Option.none);
  print = (fun _ -> assert false);
  ppname = "ignored";
  name_constr = (fun _ -> "<ignored>");
}

let check ?name ?(raise=bad_format) ?errmsg f =
  pp
    ?name
    (fun ~pos x ->
       if not (f x) then
         match errmsg with
         | Some m -> raise ~pos "%s" m
         | None -> unexpected ()
       else x)
    (fun x ->
       assert (
         f x ||
         (OpamConsole.error "Check failed on value printing%s%s"
            (match name with Some n -> " at "^n | None -> "")
            (match errmsg with Some e -> " ("^e^")" | None -> "");
          false));
       x)

let map_pair ?name ?posf1 ?posf2 (pp1: ('a,'b) t) (pp2: ('c,'d) t) =
  let name = match name with
    | None -> Printf.sprintf "(%s, %s)" pp1.ppname pp2.ppname
    | Some n -> n
  in
  pp ~name
    (fun ~pos (a,b) ->
       let posf1 = OpamStd.Option.default (fun _ -> pos) posf1 in
       parse pp1 ~pos:(posf1 a) a,
       let posf2 = OpamStd.Option.default (fun _ -> pos) posf2 in
       parse pp2 ~pos:(posf2 b) b)
    (fun (a,b) -> print pp1 a, print pp2 b)

let map_fst pp1 =
  pp
    (fun ~pos (a,b) -> pp1.parse ~pos a, b)
    (fun (a, b) -> pp1.print a, b)

let map_snd pp1 =
  pp
    (fun ~pos (a,b) -> a, pp1.parse ~pos b)
    (fun (a, b) -> a, pp1.print b)

let map_list ?name ?posf pp1 =
  let name = match name with
    | None -> pp1.ppname ^ "*"
    | Some n -> n
  in
  pp ~name
    (fun ~pos l ->
       let posf = OpamStd.Option.default (fun _ -> pos) posf in
       List.rev (List.rev_map (fun x -> parse pp1 ~pos:(posf x) x) l))
    (List.rev @* List.rev_map (print pp1))

let map_option ?name pp1 =
  let name = match name with
    | None -> pp1.ppname ^ "?"
    | Some n -> n
  in
  pp ~name
    (fun ~pos -> OpamStd.Option.map (parse pp1 ~pos))
    (OpamStd.Option.map (print pp1))

let singleton = {
  parse = (fun ~pos:_ -> function [x] -> x | _ -> unexpected ());
  print = (fun x -> [x]);
  ppname = "";
  name_constr = (fun x -> x);
}

(** Pps from strings *)

module type STR = sig
  type t
  val of_string: string -> t
  val to_string: t -> string
end

let of_module (type a) name m =
  let module X = (val m: STR with type t = a) in
  pp ~name
    (fun ~pos:_ -> X.of_string)
    X.to_string

(** Build tuples from lists *)
let (^+) pp1 pp2 =
  pp
    ~name:(Printf.sprintf "%s %s" pp1.ppname pp2.ppname)
    (fun ~pos -> function
       | x::r -> parse pp1 ~pos x, parse pp2 ~pos r
       | [] -> unexpected ())
    (fun (x,y) -> print pp1 x :: print pp2 y)

let last = singleton

let opt pp1 =
  pp
    ~name:("?"^pp1.ppname)
    (fun ~pos -> function [] -> None | l -> Some (pp1.parse ~pos l))
    (function Some x -> pp1.print x | None -> [])

let default d =
  pp
    (fun ~pos:_ -> function None -> d | Some x -> x)
    (fun x -> Some x)

let fallback pp1 pp2 =
  let parse ~pos x =
    try pp1.parse ~pos x with e ->
      OpamStd.Exn.fatal e;
      let bt = Printexc.get_raw_backtrace () in
      try pp2.parse ~pos x with _ ->
        Printexc.raise_with_backtrace e bt
  in
  { pp1 with parse }


module Op = struct
  let ( -| ) = ( -| )
  let ( ^+ ) = ( ^+ )
end

(** Pps for file contents (item lists), mostly list of [Variable(...)]
    fields *)

type ('a, 'value) field_parser = ('a * 'value option, 'a) t

(** add setter/getter and an accumulator to a pp; useful to use
    to get/set field records *)
let ppacc_opt
(* : ('a -> 'b -> 'a) -> ('a -> 'b option) -> ('value, 'b) t -> 'a field_parser *)
  = fun ?(cleanup = fun ~pos:_ _acc x -> x) set get pp1 ->
    let parse ~pos = function
      | acc, Some s ->
        set (cleanup ~pos acc (pp1.parse ~pos s)) acc
      | acc, None -> acc
    in
    let print s = s, OpamStd.Option.map pp1.print (get s) in
    {
      parse; print;
      ppname = pp1.ppname;
      name_constr = (fun x -> x);
    }

let ppacc ?cleanup set get pp =
  ppacc_opt set (fun x -> Some (get x)) ?cleanup pp

let ppacc_ignore = {
  parse = (fun ~pos:_ (acc,_) -> acc);
  print = (fun s -> s, None);
  ppname = "<ignored>";
  name_constr = (fun _ -> "<ignored>");
}

let embed set get ppacc = {
  parse = (fun ~pos (acc, x) -> set (ppacc.parse ~pos (get acc, x)) acc);
  print = (fun s -> let s1, v = ppacc.print (get s) in set s1 s, v);
  ppname = ppacc.ppname;
  name_constr = ppacc.name_constr;
}
