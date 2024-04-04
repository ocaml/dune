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

  let to_dyn =
    let open Dyn in
    function
    | Loc -> variant "Loc" []
    | Error -> variant "Error" []
    | Warning -> variant "Warning" []
    | Kwd -> variant "Kwd" []
    | Id -> variant "Id" []
    | Prompt -> variant "Prompt" []
    | Hint -> variant "Hint" []
    | Details -> variant "Details" []
    | Ok -> variant "Ok" []
    | Debug -> variant "Debug" []
    | Success -> variant "Success" []
    | Ansi_styles l -> variant "Ansi_styles" [ list Ansi_color.Style.to_dyn l ]
  ;;

  let compare t1 t2 : Ordering.t =
    match t1, t2 with
    | Loc, Loc -> Eq
    | Loc, _ -> Lt
    | _, Loc -> Gt
    | Error, Error -> Eq
    | Error, _ -> Lt
    | _, Error -> Gt
    | Warning, Warning -> Eq
    | Warning, _ -> Lt
    | _, Warning -> Gt
    | Kwd, Kwd -> Eq
    | Kwd, _ -> Lt
    | _, Kwd -> Gt
    | Id, Id -> Eq
    | Id, _ -> Lt
    | _, Id -> Gt
    | Prompt, Prompt -> Eq
    | Prompt, _ -> Lt
    | _, Prompt -> Gt
    | Hint, Hint -> Eq
    | Hint, _ -> Lt
    | _, Hint -> Gt
    | Details, Details -> Eq
    | Details, _ -> Lt
    | _, Details -> Gt
    | Ok, Ok -> Eq
    | Ok, _ -> Lt
    | _, Ok -> Gt
    | Debug, Debug -> Eq
    | Debug, _ -> Lt
    | _, Debug -> Gt
    | Success, Success -> Eq
    | Success, _ -> Lt
    | _, Success -> Gt
    | Ansi_styles _, Ansi_styles _ -> Eq
  ;;
end

module Annots = struct
  module Info = struct
    module Id = struct
      type 'a t =
        { id : 'a Type_eq.Id.t
        ; name : string
        }

      module Packed = struct
        type 'a unpacked = 'a t
        type t = Id : 'a unpacked -> t

        let equal (Id { id; name }) (Id t) =
          Type_eq.Id.equal id t.id && String.equal name t.name
        ;;

        let hash (Id { id; name }) = Tuple.T2.hash Type_eq.Id.hash String.hash (id, name)
        let to_dyn (Id { name; _ }) = Dyn.variant "Info.Id" [ Dyn.string name ]
      end
    end

    type 'a info =
      { id : 'a Id.t
      ; to_dyn : 'a -> Dyn.t
      }

    type packed_info = E : 'a info -> packed_info

    let all : (Id.Packed.t, packed_info) Table.t = Table.create (module Id.Packed) 12

    (* morally, this should be ['a info], but we need all this circus to make
       sure we don't store functions in the map's keys so that it remains
       marshabllable *)
    type 'a t = 'a Id.t

    let to_dyn : 'a. 'a t -> 'a -> Dyn.t =
      fun (type a) (info : a t) (a : a) ->
      let (E packed) = Table.find_exn all (Id.Packed.Id info) in
      match Type_eq.Id.same info.id packed.id.id with
      | Some eq -> packed.to_dyn (Type_eq.cast eq a)
      | None ->
        Code_error.raise
          "type id's disagree for the same name"
          [ "info.name", Dyn.string info.name ]
    ;;

    let create ~name to_dyn =
      let type_id = Type_eq.Id.create () in
      let id = { Id.id = type_id; name } in
      let info = { id; to_dyn } in
      Table.add_exn all (Id.Packed.Id id) (E info);
      id
    ;;
  end

  module T = Univ_map.Make (Info) ()

  module Key = struct
    include T.Key

    let create ~name to_dyn = create (Info.create ~name to_dyn)
  end

  include (T : Univ_map.S with type t = T.t and module Key := Key)

  let has_embedded_location = Key.create ~name:"has-embedded-location" Unit.to_dyn
  let needs_stack_trace = Key.create ~name:"needs-stack-trace" Unit.to_dyn

  let to_dyn t =
    Dyn.Map
      (let f =
         { T.fold =
             (fun (info : _ Info.t) a acc ->
               (Dyn.string info.name, Info.to_dyn info a) :: acc)
         }
       in
       T.fold t ~init:[] ~f)
  ;;
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
  ;;
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
;;

let equal a b = Ordering.is_eq (compare a b)

let make ?loc ?prefix ?(hints = []) ?(annots = Annots.empty) paragraphs =
  let paragraphs =
    match prefix, paragraphs with
    | None, l -> l
    | Some p, [] -> [ p ]
    | Some p, x :: l -> Pp.concat ~sep:Pp.space [ p; x ] :: l
  in
  { loc; hints; paragraphs; annots }
;;

let pp { loc; paragraphs; hints; annots = _ } =
  let open Pp.O in
  let paragraphs =
    match hints with
    | [] -> paragraphs
    | _ ->
      List.append
        paragraphs
        (List.map hints ~f:(fun hint ->
           Pp.tag Style.Hint (Pp.verbatim "Hint:") ++ Pp.space ++ hint))
  in
  let paragraphs = List.map paragraphs ~f:Pp.box in
  let paragraphs =
    match loc with
    | None -> paragraphs
    | Some loc ->
      let start = Loc0.start loc in
      let stop = Loc0.stop loc in
      let start_c = start.pos_cnum - start.pos_bol in
      let stop_c = stop.pos_cnum - start.pos_bol in
      let lnum =
        if start.pos_lnum = stop.pos_lnum
        then Printf.sprintf "line %d" start.pos_lnum
        else Printf.sprintf "lines %d-%d" start.pos_lnum stop.pos_lnum
      in
      Pp.box
        (Pp.tag
           Style.Loc
           (Pp.textf "File %S, %s, characters %d-%d:" start.pos_fname lnum start_c stop_c))
      :: paragraphs
  in
  Pp.vbox (Pp.concat_map paragraphs ~sep:Pp.nop ~f:(fun pp -> Pp.seq pp Pp.cut))
;;

let print ?(config = Print_config.default) t =
  Ansi_color.print (Pp.map_tags (pp t) ~f:config)
;;

let prerr ?(config = Print_config.default) t =
  Ansi_color.prerr (Pp.map_tags (pp t) ~f:config)
;;

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
      if s.[i - 1] = t.[j - 1]
      then d.(i).(j) <- d.(i - 1).(j - 1) (* no operation required *)
      else
        d.(i).(j)
        <- min
             (d.(i - 1).(j) + 1) (* a deletion *)
             (min
                (d.(i).(j - 1) + 1) (* an insertion *)
                (d.(i - 1).(j - 1) + 1) (* a substitution *))
    done
  done;
  d.(m).(n)
;;

let did_you_mean s ~candidates =
  let candidates =
    List.filter candidates ~f:(fun candidate -> levenshtein_distance s candidate < 3)
  in
  match candidates with
  | [] -> []
  | l -> [ Pp.textf "did you mean %s?" (String.enumerate_or l) ]
;;

let to_string t =
  let full_error = Format.asprintf "%a" Pp.to_fmt (pp { t with loc = None }) in
  match String.drop_prefix ~prefix:"Error: " full_error with
  | None -> full_error
  | Some error -> String.trim error
;;

let is_loc_none loc =
  match loc with
  | None -> true
  | Some loc -> loc = Loc0.none
;;

let has_embedded_location msg = Annots.mem msg.annots Annots.has_embedded_location
let has_location msg = (not (is_loc_none msg.loc)) || has_embedded_location msg
let needs_stack_trace msg = Annots.mem msg.annots Annots.needs_stack_trace

let command cmd =
  (* CR-someday rgrinberg: this should be its own tag, but that might bring
     some backward compat issues with rpc. *)
  Pp.concat
    [ Pp.verbatim "'"
    ; Pp.tag (Style.Ansi_styles [ `Underline ]) @@ Pp.verbatim cmd
    ; Pp.verbatim "'"
    ]
;;
