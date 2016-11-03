open Import
open Future

module Spec = struct
  type _ t =
    | Unit : string list -> unit t
    | Vals : 'a Values.Spec.t -> 'a Values.t t
    | Both : string list * 'a Values.Spec.t -> 'a Values.t t

  let filenames : type a. a t -> String_set.t = function
    | Unit fns -> String_set.of_list fns
    | Vals vals -> String_set.of_list (Values.Spec.filenames vals)
    | Both (fns, vals) ->
      String_set.union
        (String_set.of_list fns)
        (String_set.of_list (Values.Spec.filenames vals))
end

type 'a with_dynamic_deps =
    Dyn : { deps : 'b Spec.t
          ; exec : 'b -> 'a Future.t
          } -> 'a with_dynamic_deps

type t =
  { deps    : String_set.t
  ; targets : String_set.t
  ; exec    : unit Future.t Lazy.t
  }

module File_kind = struct
  type 'a t =
    | Ignore_contents : unit t
    | Sexp_file : 'a Kind.t -> 'a t

  let eq : type a b. a t -> b t -> (a, b) eq = fun a b ->
    match a, b with
    | Ignore_contents, Ignore_contents -> Eq
    | Sexp_file a    , Sexp_file b     -> Kind.eq a b
    | _                                -> Ne
end

type file_spec =
    F : { rule         : t (* Rule which produces it *)
        ; kind         : 'a File_kind.t
        ; mutable data : 'a option
        }
    -> file_spec

(* File specification by targets *)
let files : (string, file_spec) Hashtbl.t = Hashtbl.create 1024

(* Union of all the dependencies all rules *)
let all_deps = ref String_set.empty

(* All files we know how to build *)
let buildable_files = ref String_set.empty

let add_files cell filenames = cell := String_set.union !cell filenames

let wait_for : type a. string -> a File_kind.t -> a Future.t = fun path kind ->
  let (F file) = Hashtbl.find files path in
  match File_kind.eq kind file.kind with
  | Ne -> assert false
  | Eq ->
    Lazy.force file.rule.exec >>= fun () ->
    match file.data with
    | Some x -> return x
    | None   -> assert false

let wait_for_file path = wait_for path Ignore_contents

let wait_for_files paths = Future.all_unit (List.map paths ~f:wait_for_file)

let rec wait_for_values : type a. a Values.Spec.t -> a Values.t Future.t =
  let open Values.Spec in
  function
  | [] -> return Values.[]
  | (path, kind) :: spec ->
    let rest = wait_for_values spec in
    wait_for path (Sexp_file kind) >>= fun x ->
    rest >>= fun l ->
    return Values.(x :: l)

let set_data : type a. string -> a File_kind.t -> a -> unit = fun path kind x ->
  let (F file) = Hashtbl.find files path in
  match File_kind.eq kind file.kind with
  | Ne -> assert false
  | Eq -> file.data <- Some x

let rec store_all_values : type a. a Values.Spec.t -> a Values.t -> unit =
  let open Values      in
  let open Values.Spec in
  fun spec vals ->
    match spec, vals with
    | [], [] -> ()
    | (path, kind) :: spec, x :: vals ->
      Kind.save kind ~filename:path x;
      set_data path (Sexp_file kind) x;
      store_all_values spec vals

let store_all_files fns =
  List.iter fns ~f:(fun fn -> set_data fn Ignore_contents ())

let store_result : type a. a Spec.t -> a -> unit = fun spec result ->
  let open Spec in
  match spec with
  | Unit fns -> store_all_files fns
  | Vals vals -> store_all_values vals result
  | Both (fns, vals) ->
    store_all_files fns;
    store_all_values vals result

let rec create_file_specs_for_values : type a. a Values.Spec.t -> t -> unit =
  let open Values.Spec in
  fun spec rule ->
    match spec with
    | [] -> ()
    | (path, kind) :: spec ->
      Hashtbl.add files ~key:path ~data:(F { kind = Sexp_file kind; rule; data = None });
      create_file_specs_for_values spec rule

let create_file_specs_for_files fns rule =
  List.iter fns ~f:(fun fn ->
    Hashtbl.add files ~key:fn ~data:(F { rule; kind = Ignore_contents; data = None }))

let create_file_specs : type a. a Spec.t -> t -> unit =
  let open Spec in
  fun spec rule ->
    match spec with
    | Unit fns -> create_file_specs_for_files fns rule
    | Vals vals -> create_file_specs_for_values vals rule
    | Both (fns, vals) ->
      create_file_specs_for_files fns rule;
      create_file_specs_for_values vals rule

let wait_for_deps : type a. a Spec.t -> a Future.t =
  let open Spec in
  function
  | Unit fns -> wait_for_files fns
  | Vals vals -> wait_for_values vals
  | Both (fns, vals) ->
    let vals = wait_for_values vals in
    wait_for_files fns >>= fun () ->
    vals

let no_more_rules_allowed = ref false

let dyn_rule ~deps ~targets f =
  assert (not !no_more_rules_allowed);
  let fdeps    = Spec.filenames deps    in
  let ftargets = Spec.filenames targets in
  add_files all_deps        fdeps;
  add_files buildable_files ftargets;
  let exec = lazy (
    wait_for_deps deps >>= fun x ->
    let (Dyn { deps; exec }) = f x in
    wait_for_deps deps >>= fun x ->
    exec x >>= fun result ->
    store_result targets result;
    return ()
  ) in
  let rule = { deps = fdeps; targets = ftargets; exec } in
  create_file_specs targets rule

let rule ~deps ~targets f =
  dyn_rule ~deps ~targets (fun x ->
    Dyn { deps = Unit []
        ; exec = (fun () -> f x)
        })

let simple_rule ~deps ?(targets=[]) ?stdout_to prog args =
  let targets =
    match stdout_to with
    | None -> targets
    | Some fn -> fn :: targets
  in
  rule ~deps:(Unit deps) ~targets:(Unit targets) (fun () ->
    run ?stdout_to prog args)

let setup_copy_rules () =
  let copy = if Sys.win32 then "copy" else "cp" in
  String_set.iter (String_set.union !all_deps !buildable_files) ~f:(fun fn ->
    if Sys.file_exists fn then
      let src = "../" ^ fn in
      simple_rule ~deps:[src] ~targets:[fn]
        copy [src; fn]
  )

let do_build targets =
  setup_copy_rules ();
  no_more_rules_allowed := true;
  wait_for_files targets
