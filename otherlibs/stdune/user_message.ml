module Style = struct
  type t =
    | Loc
    | Error
    | Warning
    | Kwd
    | Id
    | Prompt
    | Hint
    | Details
    | Ok
    | Debug
    | Success
    | Ansi_styles of Ansi_color.Style.t list
end

module Annots = struct
  include Univ_map.Make ()

  let has_embedded_location =
    Key.create ~name:"has-embedded-location" Unit.to_dyn

  let needs_stack_trace = Key.create ~name:"needs-stack-trace" Unit.to_dyn
end

module Print_config = struct
  type t = Style.t -> Ansi_color.Style.t list

  let default : t = function
    | Loc -> [ `Bold ]
    | Error -> [ `Bold; `Fg_red ]
    | Warning -> [ `Bold; `Fg_magenta ]
    | Kwd -> [ `Bold; `Fg_blue ]
    | Id -> [ `Bold; `Fg_yellow ]
    | Prompt -> [ `Bold; `Fg_green ]
    | Hint -> [ `Italic; `Fg_white ]
    | Details -> [ `Dim; `Fg_white ]
    | Ok -> [ `Dim; `Fg_green ]
    | Debug -> [ `Underline; `Fg_bright_cyan ]
    | Success -> [ `Bold; `Fg_green ]
    | Ansi_styles l -> l
end

type t =
  { loc : Loc0.t option
  ; paragraphs : Style.t Pp.t list
  ; hints : Style.t Pp.t list
  ; annots : Annots.t
  }

let compare { loc; paragraphs; hints; annots } t =
  let open Ordering.O in
  let= () = Option.compare Loc0.compare loc t.loc in
  let= () = List.compare paragraphs t.paragraphs ~compare:Poly.compare in
  let= () = List.compare hints t.hints ~compare:Poly.compare in
  Poly.compare annots t.annots

let equal a b = Ordering.is_eq (compare a b)

let make ?loc ?prefix ?(hints = []) ?(annots = Annots.empty) paragraphs =
  let paragraphs =
    match (prefix, paragraphs) with
    | None, l -> l
    | Some p, [] -> [ p ]
    | Some p, x :: l -> Pp.concat ~sep:Pp.space [ p; x ] :: l
  in
  { loc; hints; paragraphs; annots }

let pp { loc; paragraphs; hints; annots = _ } =
  let open Pp.O in
  let paragraphs =
    match hints with
    | [] -> paragraphs
    | _ ->
      List.append paragraphs
        (List.map hints ~f:(fun hint ->
             Pp.tag Style.Hint (Pp.verbatim "Hint:") ++ Pp.space ++ hint))
  in
  let paragraphs = List.map paragraphs ~f:Pp.box in
  let paragraphs =
    match loc with
    | None -> paragraphs
    | Some { Loc0.start; stop } ->
      let start_c = start.pos_cnum - start.pos_bol in
      let stop_c = stop.pos_cnum - start.pos_bol in
      Pp.box
        (Pp.tag Style.Loc
           (Pp.textf "File %S, line %d, characters %d-%d:" start.pos_fname
              start.pos_lnum start_c stop_c))
      :: paragraphs
  in
  Pp.vbox (Pp.concat_map paragraphs ~sep:Pp.nop ~f:(fun pp -> Pp.seq pp Pp.cut))

let print ?(config = Print_config.default) t =
  Ansi_color.print (Pp.map_tags (pp t) ~f:config)

let prerr ?(config = Print_config.default) t =
  Ansi_color.prerr (Pp.map_tags (pp t) ~f:config)

(* As found here http://rosettacode.org/wiki/Levenshtein_distance#OCaml *)
let levenshtein_distance s t =
  let m = String.length s
  and n = String.length t in
  (* for all i and j, d.(i).(j) will hold the Levenshtein distance between the
     first i characters of s and the first j characters of t *)
  let d = Array.make_matrix ~dimx:(m + 1) ~dimy:(n + 1) 0 in
  for i = 0 to m do
    (* the distance of any first string to an empty second string *)
    d.(i).(0) <- i
  done;
  for j = 0 to n do
    (* the distance of any second string to an empty first string *)
    d.(0).(j) <- j
  done;
  for j = 1 to n do
    for i = 1 to m do
      if s.[i - 1] = t.[j - 1] then d.(i).(j) <- d.(i - 1).(j - 1)
        (* no operation required *)
      else
        d.(i).(j) <-
          min
            (d.(i - 1).(j) + 1) (* a deletion *)
            (min
               (d.(i).(j - 1) + 1) (* an insertion *)
               (d.(i - 1).(j - 1) + 1) (* a substitution *))
    done
  done;
  d.(m).(n)

let did_you_mean s ~candidates =
  let candidates =
    List.filter candidates ~f:(fun candidate ->
        levenshtein_distance s candidate < 3)
  in
  match candidates with
  | [] -> []
  | l -> [ Pp.textf "did you mean %s?" (String.enumerate_or l) ]

let to_string t =
  let full_error = Format.asprintf "%a" Pp.to_fmt (pp { t with loc = None }) in
  match String.drop_prefix ~prefix:"Error: " full_error with
  | None -> full_error
  | Some error -> String.trim error

let is_loc_none loc =
  match loc with
  | None -> true
  | Some loc -> loc = Loc0.none

let has_embedded_location msg =
  Annots.mem msg.annots Annots.has_embedded_location

let has_location msg = (not (is_loc_none msg.loc)) || has_embedded_location msg

let needs_stack_trace msg = Annots.mem msg.annots Annots.needs_stack_trace
