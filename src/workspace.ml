open Import
open Stanza.Of_sexp

(* workspace files use the same version numbers as dune-project files
   for simplicity *)
let syntax = Stanza.syntax

module Context = struct
  module Target = struct
    type t =
      | Native
      | Named of string

    let t =
      map string ~f:(function
        | "native" -> Native
        | s        -> Named s)

    let add ts x =
      match x with
      | None -> ts
      | Some t ->
        if List.mem t ~set:ts then
          ts
        else
          ts @ [t]
  end

  module Name = struct
    let t =
      plain_string (fun ~loc name ->
        if name = "" ||
           String.is_prefix name ~prefix:"." ||
           name = "log" ||
           name = "install" ||
           String.contains name '/' ||
           String.contains name '\\' then
          of_sexp_errorf loc
            "%S is not allowed as a build context name" name;
        name)
  end

  module Opam = struct
    type t =
      { loc     : Loc.t
      ; name    : string
      ; profile : string
      ; switch  : string
      ; root    : string option
      ; merlin  : bool
      ; targets : Target.t list
      }

    let t ~profile ~x =
      field   "switch"  string                                    >>= fun switch ->
      field   "name"    Name.t ~default:switch                    >>= fun name ->
      field   "targets" (list Target.t) ~default:[Target.Native]  >>= fun targets ->
      field_o "root"    string                                    >>= fun root ->
      field_b "merlin"                                            >>= fun merlin ->
      field   "profile" string ~default:profile                   >>= fun profile ->
      loc >>= fun loc ->
      return { loc
             ; switch
             ; name
             ; root
             ; merlin
             ; targets = Target.add targets x
             ; profile
             }
  end

  module Default = struct
    type t =
      { loc     : Loc.t
      ; profile : string
      ; targets : Target.t list
      }

    let t ~profile ~x =
      field "targets" (list Target.t) ~default:[Target.Native]
      >>= fun targets ->
      field "profile" string ~default:profile
      >>= fun profile ->
      loc
      >>= fun loc ->
      return { loc
             ; targets = Target.add targets x
             ; profile
             }
  end

  type t = Default of Default.t | Opam of Opam.t

  let loc = function
    | Default x -> x.loc
    | Opam    x -> x.loc

  let t ~profile ~x =
    sum
      [ "default",
        (fields (Default.t ~profile ~x) >>| fun x ->
         Default x)
      ; "opam",
        (fields (Opam.t ~profile ~x) >>| fun x ->
         Opam x)
      ]

  let t ~profile ~x =
    Syntax.get_exn syntax >>= function
    | (0, _) ->
      (* jbuild-workspace files *)
      (peek_exn >>= function
       | List (_, List _ :: _) ->
         Sexp.Of_sexp.record (Opam.t ~profile ~x) >>| fun x -> Opam x
       | _ -> t ~profile ~x)
    | _ -> t ~profile ~x

  let name = function
    | Default _ -> "default"
    | Opam    o -> o.name

  let targets = function
    | Default x -> x.targets
    | Opam    x -> x.targets

  let all_names t =
    let n = name t in
    n :: List.filter_map (targets t) ~f:(function
      | Native -> None
      | Named s -> Some (n ^ "." ^ s))

  let default ?x ?profile () =
    Default
      { loc = Loc.of_pos __POS__
      ; targets = [Option.value x ~default:Target.Native]
      ; profile = Option.value profile
                    ~default:Config.default_build_profile
      }
end

type t =
  { merlin_context : string option
  ; contexts       : Context.t list
  }

include Versioned_file.Make(struct type t = unit end)
let () = Lang.register syntax ()

let t ?x ?profile:cmdline_profile () =
  field "profile" string ~default:Config.default_build_profile
  >>= fun profile ->
  let profile = Option.value cmdline_profile ~default:profile in
  multi_field "context" (Context.t ~profile ~x)
  >>= fun contexts ->
  let defined_names = ref String.Set.empty in
  let { merlin_context; contexts } =
    let init =
      { merlin_context = None
      ; contexts       = []
      }
    in
    List.fold_left contexts ~init ~f:(fun t ctx ->
      let name = Context.name ctx in
      if String.Set.mem !defined_names name then
        Loc.fail (Context.loc ctx)
          "second definition of build context %S" name;
      defined_names := String.Set.union !defined_names
                         (String.Set.of_list (Context.all_names ctx));
      match ctx, t.merlin_context with
      | Opam { merlin = true; _ }, Some _ ->
        Loc.fail (Context.loc ctx)
          "you can only have one context for merlin"
      | Opam { merlin = true; _ }, None ->
        { merlin_context = Some name; contexts = ctx :: t.contexts }
      | _ ->
        { t with contexts = ctx :: t.contexts })
  in
  let contexts =
    match contexts with
    | [] -> [Context.default ?x ~profile ()]
    | _  -> contexts
  in
  let merlin_context =
    match merlin_context with
    | Some _ -> merlin_context
    | None ->
      if List.exists contexts
           ~f:(function Context.Default _ -> true | _ -> false) then
        Some "default"
      else
        None
  in
  return
    { merlin_context
    ; contexts = List.rev contexts
    }

let t ?x ?profile () = fields (t ?x ?profile ())

let default ?x ?profile () =
  { merlin_context = Some "default"
  ; contexts = [Context.default ?x ?profile ()]
  }

let load ?x ?profile p =
  let x = Option.map x ~f:(fun s -> Context.Target.Named s) in
  match Which_program.t with
  | Dune ->
    Io.with_lexbuf_from_file p ~f:(fun lb ->
      if Dune_lexer.eof_reached lb then
        default ?x ?profile ()
      else
        let first_line = Dune_lexer.first_line lb in
        parse_contents lb first_line ~f:(fun _lang -> t ?x ?profile ()))
  | Jbuilder ->
    let sexp =
      Io.Sexp.load p ~mode:Many_as_one ~lexer:Sexp.Lexer.jbuild_token
    in
    parse
      (enter (t ?x ?profile ()))
      (Univ_map.singleton (Syntax.key syntax) (0, 0))
      sexp

let default ?x ?profile () =
  let x = Option.map x ~f:(fun s -> Context.Target.Named s) in
  default ?x ?profile ()

let filename =
  match Which_program.t with
  | Dune     -> "dune-workspace"
  | Jbuilder -> "jbuild-workspace"
