open Import
module P = Ocaml.Variant
module Ps = Ocaml.Variant.Set

let meta_fn = "META"
let findlib_predicates_set_by_dune = Ps.of_list [ P.ppx_driver; P.mt; P.mt_posix ]

type t =
  { meta_file : Path.t
  ; name : Lib_name.t
  ; dir : Path.t
  ; vars : Vars.t
  }

let preds = findlib_predicates_set_by_dune

let get_paths t var preds =
  List.map (Vars.get_words t.vars var preds) ~f:(Path.relative t.dir)
;;

let make_archives t var preds =
  Mode.Dict.of_func (fun ~mode -> get_paths t var (Ps.add preds (Mode.variant mode)))
;;

let version t = Vars.get t.vars "version" Ps.empty
let description t = Vars.get t.vars "description" Ps.empty
let jsoo_runtime t = get_paths t "jsoo_runtime" Ps.empty

let requires t =
  Vars.get_words t.vars "requires" preds
  |> List.map ~f:(fun s -> Lib_name.parse_string_exn (Loc.none, s))
;;

let ppx_runtime_deps t =
  Vars.get_words t.vars "ppx_runtime_deps" preds
  |> List.map ~f:(fun s -> Lib_name.parse_string_exn (Loc.none, s))
;;

let kind t : Lib_kind.t =
  match Vars.get t.vars "library_kind" Ps.empty with
  | Some "ppx_rewriter" -> Ppx_rewriter Lib_kind.Ppx_args.empty
  | Some "ppx_deriver" -> Ppx_deriver Lib_kind.Ppx_args.empty
  | None | Some _ -> Normal
;;

let archives t = make_archives t "archive" preds

let plugins t =
  Mode.Dict.map2
    ~f:( @ )
    (make_archives t "archive" (Ps.add preds Variant.plugin))
    (make_archives t "plugin" preds)
;;

module Exists
    (Monad : sig
       type 'a t

       val return : 'a -> 'a t

       module List : sig
         val for_all : 'a list -> f:('a -> bool t) -> bool t
         val exists : 'a list -> f:('a -> bool t) -> bool t
       end
     end)
    (Fs : sig
       val file_exists : Path.t -> bool Monad.t
     end) =
struct
  let exists t ~is_builtin =
    let exists_if = Vars.get_words t.vars "exists_if" Ps.empty in
    match exists_if with
    | _ :: _ ->
      Monad.List.for_all exists_if ~f:(fun fn -> Fs.file_exists (Path.relative t.dir fn))
    | [] ->
      if not is_builtin
      then Monad.return true
      else (
        (* The META files for installed packages are sometimes broken, i.e.
           META files for libraries that were not installed by the compiler
           are still present:

           https://github.com/ocaml/dune/issues/563

           To workaround this problem, for builtin packages we check that at
           least one of the archive is present. *)
        match archives t with
        | { byte = []; native = [] } -> Monad.return true
        | { byte; native } -> Monad.List.exists (byte @ native) ~f:Fs.file_exists)
  ;;
end

let candidates ~dir name =
  [ meta_fn ^ "." ^ Package.Name.to_string name; meta_fn ]
  |> List.map ~f:(Path.Outside_build_dir.relative dir)
;;
