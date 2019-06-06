module Style = struct
  type t =
    | Loc
    | Error
    | Warning
    | Kwd
    | Id
    | Prompt
    | Details
    | Ok
    | Debug
end

module Print_config = struct
  type t = Style.t -> Ansi_color.Style.t list

  let default : t = function
    | Loc     -> [Bold]
    | Error   -> [Bold; Fg Red]
    | Warning -> [Bold; Fg Magenta]
    | Kwd     -> [Bold; Fg Blue]
    | Id      -> [Bold; Fg Yellow]
    | Prompt  -> [Bold; Fg Green]
    | Details -> [Dim; Fg White]
    | Ok      -> [Dim; Fg Green]
    | Debug   -> [Underlined; Fg Bright_cyan]
end

type t =
  { loc : Loc0.t option
  ; paragraphs : Style.t Pp.t list
  ; hints : Style.t Pp.t list
  }

let make ?loc ?prefix ?(hints=[]) paragraphs =
  let paragraphs =
    match prefix, paragraphs with
    | None, l -> l
    | Some p, [] -> [p]
    | Some p, x :: l ->
      Pp.concat ~sep:Pp.space [p; x] :: l
  in
  { loc; hints; paragraphs }

let pp { loc; paragraphs; hints } =
  let paragraphs =
    match hints with
    | [] -> paragraphs
    | _ ->
      List.append
        paragraphs
        (List.map hints ~f:(fun hint ->
           Pp.concat ~sep:Pp.space [Pp.verbatim "Hint:"; hint]))
  in
  let paragraphs = List.map paragraphs ~f:Pp.box in
  let paragraphs =
    match loc with
    | None -> paragraphs
    | Some { Loc0.start; stop } ->
      let start_c = start.pos_cnum - start.pos_bol in
      let stop_c  = stop.pos_cnum  - start.pos_bol in
      Pp.tag ~tag:Style.Loc
        (Pp.textf "File %S, line %d, characters %d-%d:"
           start.pos_fname start.pos_lnum start_c stop_c)
      :: paragraphs
  in
  Pp.vbox (Pp.concat paragraphs ~sep:Pp.cut)

let print ?(config=Print_config.default) ?margin t =
  Ansi_color.print ?margin (Pp.map_tags (pp t) ~f:config)

let prerr ?(config=Print_config.default) ?margin t =
  Ansi_color.prerr ?margin (Pp.map_tags (pp t) ~f:config)

(* As found here http://rosettacode.org/wiki/Levenshtein_distance#OCaml *)
let levenshtein_distance s t =
  let m = String.length s
  and n = String.length t in
  (* for all i and j, d.(i).(j) will hold the Levenshtein distance between
     the first i characters of s and the first j characters of t *)
  let d = Array.make_matrix ~dimx:(m+1) ~dimy:(n+1) 0 in

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

      if s.[i-1] = t.[j-1] then
        d.(i).(j) <- d.(i-1).(j-1)  (* no operation required *)
      else
        d.(i).(j) <- min
                       (d.(i-1).(j) + 1) (* a deletion *)
                       (min
                          (d.(i).(j-1) + 1) (* an insertion *)
                          (d.(i-1).(j-1) + 1) (* a substitution *)
                       )
    done;
  done;

  d.(m).(n)

let did_you_mean s ~candidates =
  let candidates =
    List.filter candidates ~f:(fun candidate ->
      levenshtein_distance s candidate < 3)
  in
  match candidates with
  | [] -> []
  | l -> [Pp.textf "did you mean %s?" (String.enumerate_or l)]
