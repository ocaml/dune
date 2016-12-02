open Import
open Future

module Pset = Path.Set

module Vspec = struct
  type 'a t = T : Path.t * 'a Vfile_kind.t -> 'a t
end

module Exec_status = struct
  type t =
    | Not_started of (targeting:Path.t -> unit Future.t)
    | Starting of { for_file : Path.t }
    | Running of { for_file : Path.t; future : unit Future.t }
end

type t =
  { deps         : Pset.t
  ; targets      : Pset.t
  ; lib_deps     : String_set.t
  ; mutable exec : Exec_status.t
  }

module File_kind = struct
  type 'a t =
    | Ignore_contents : unit t
    | Sexp_file       : 'a Vfile_kind.t -> 'a t

  let eq : type a b. a t -> b t -> (a, b) eq option = fun a b ->
    match a, b with
    | Ignore_contents, Ignore_contents -> Some Eq
    | Sexp_file a    , Sexp_file b     -> Vfile_kind.eq a b
    | _                                -> None

  let eq_exn a b = Option.value_exn (eq a b)
end

module File_spec = struct
  type rule = t
  type 'a t =
    { rule         : rule (* Rule which produces it *)
    ; mutable kind : 'a File_kind.t
    ; mutable data : 'a option
    }

  type packed = T : _ t -> packed

  let create rule kind =
    T { rule; kind; data = None }
end

(* File specification by targets *)
let files : (Path.t, File_spec.packed) Hashtbl.t = Hashtbl.create 1024

(* Union of all the local dependencies of all rules *)
let all_deps = ref Pset.empty

(* All files we know how to build *)
let buildable_files = ref Pset.empty

let add_files cell filenames = cell := Pset.union filenames !cell

let find_file_exn file =
  Hashtbl.find_exn files file ~string_of_key:(fun fn -> sprintf "%S" (Path.to_string fn))
    ~table_desc:(fun _ -> "<target to rule>")

module Build_error = struct
  type t =
    { backtrace : Printexc.raw_backtrace
    ; dep_path  : Path.t list
    ; exn       : exn
    }

  let backtrace t = t.backtrace
  let dependency_path t = t.dep_path
  let exn t = t.exn

  exception E of t

  let raise ~targeting exn =
    let backtrace = Printexc.get_raw_backtrace () in
    let rec build_path acc targeting ~seen =
      assert (not (Pset.mem targeting seen));
      let seen = Pset.add targeting seen in
      let (File_spec.T file) = find_file_exn targeting in
      match file.rule.exec with
      | Not_started _ -> assert false
      | Running { for_file; _ } | Starting { for_file } ->
        if for_file = targeting then
          acc
        else
          build_path (for_file :: acc) for_file ~seen
    in
    let dep_path = build_path [targeting] targeting ~seen:Pset.empty in
    raise (E { backtrace; dep_path; exn })
end

let wait_for_file fn ~targeting =
  match Hashtbl.find files fn with
  | None ->
    if Path.is_in_build_dir fn then
      die "no rule found for %s" (Path.to_string fn)
    else if Path.exists fn then
      return ()
    else
      die "file unavailable: %s" (Path.to_string fn)
  | Some (File_spec.T file) ->
    match file.rule.exec with
    | Not_started f ->
      file.rule.exec <- Starting { for_file = targeting };
      let future =
        try
          f ~targeting:fn
        with
        | Build_error.E _ as exn -> raise exn
        | exn ->
          Build_error.raise ~targeting:fn exn
      in
      file.rule.exec <- Running { for_file = targeting; future };
      future
    | Running { future; _ } -> future
    | Starting _ ->
      (* Recursive deps! *)
      let rec build_loop acc targeting =
        let acc = targeting :: acc in
        if fn = targeting then
          acc
        else
          let (File_spec.T file) = find_file_exn targeting in
          match file.rule.exec with
          | Not_started _ | Running _ -> assert false
          | Starting { for_file } ->
            build_loop acc for_file
      in
      let loop = build_loop [fn] targeting in
      die "Depency cycle between the following files:\n    %s"
        (String.concat ~sep:"\n--> "
           (List.map loop ~f:Path.to_string))

module Target = struct
  type t =
    | Normal of Path.t
    | Vfile : _ Vspec.t -> t

  let paths ts =
    List.fold_left ts ~init:Pset.empty ~f:(fun acc t ->
      match t with
      | Normal p -> Pset.add p acc
      | Vfile (Vspec.T (fn, _)) -> Pset.add fn acc)
end

module Prog_spec = struct
  type 'a t =
    | Dep of Path.t
    | Dyn of ('a -> Path.t)
end

module Build = struct
  type ('a, 'b) t =
    | Arr : ('a -> 'b) -> ('a, 'b) t
    | Prim : { targets : Target.t list; exec : 'a -> 'b Future.t } -> ('a, 'b) t
    | Compose : ('a, 'b) t * ('b, 'c) t -> ('a, 'c) t
    | First : ('a, 'b) t -> ('a * 'c, 'b * 'c) t
    | Second : ('a, 'b) t -> ('c * 'a, 'c * 'b) t
    | Split : ('a, 'b) t * ('c, 'd) t -> ('a * 'c, 'b * 'd) t
    | Fanout : ('a, 'b) t * ('a, 'c) t -> ('a, 'b * 'c) t
    | Paths : Pset.t -> ('a, 'a) t
    | Vpath : 'a Vspec.t -> (unit, 'a) t
    | Dyn_paths : ('a, Path.t list) t -> ('a, 'a) t
    | Record_lib_deps : string list -> ('a, 'a) t

  let arr f = Arr f
  let return x = Arr (fun () -> x)

  let record_lib_deps names = Record_lib_deps names

  module O = struct
    let ( >>> ) a b =
      match a, b with
      | Arr a, Arr b -> Arr (fun x -> (b (a x)))
      | _ -> Compose (a, b)

    let ( >>^ ) t f = t >>> arr f
    let ( ^>> ) f t = arr f >>> t

    let ( *** ) a b = Split (a, b)
    let ( &&& ) a b = Fanout (a, b)
  end
  open O

  let first t = First t
  let second t = Second t
  let fanout a b = Fanout (a, b)
  let fanout3 a b c =
    let open O in
    (a &&& (b &&& c))
    >>>
    arr (fun (a, (b, c)) -> (a, b, c))

  let rec all = function
    | [] -> arr (fun _ -> [])
    | t :: ts ->
      t &&& all ts
      >>>
      arr (fun (x, y) -> x :: y)

  let path p = Paths (Pset.singleton p)
  let paths ps = Paths (Pset.of_list ps)
  let path_set ps = Paths ps
  let vpath vp = Vpath vp
  let dyn_paths t = Dyn_paths t

  let prim ~targets exec = Prim { targets; exec }

  let create_files ~targets exec =
    let targets = List.map targets ~f:(fun t -> Target.Normal t) in
    prim ~targets (fun x -> Future.return (exec x))
  let create_file ~target exec =
    create_files ~targets:[target] exec

  let get_file : type a. Path.t -> a File_kind.t -> a File_spec.t = fun fn kind ->
    match Hashtbl.find files fn with
    | None -> die "no rule found for %s" (Path.to_string fn)
    | Some (File_spec.T file) ->
      let Eq = File_kind.eq_exn kind file.kind in
      file

  let save_vfile (type a) (module K : Vfile_kind.S with type t = a) fn x =
    K.save x ~filename:(Path.to_string fn)

  let store_vfile spec =
    prim ~targets:[Vfile spec] (fun x ->
      let (Vspec.T (fn, kind)) = spec in
      let file = get_file fn (Sexp_file kind) in
      assert (file.data = None);
      file.data <- Some x;
      save_vfile kind fn x;
      Future.return ())

  let get_prog (prog : _ Prog_spec.t) =
    match prog with
    | Dep p -> path p >>> arr (fun _ -> p)
    | Dyn f -> arr f >>> dyn_paths (arr (fun x -> [x]))

  let prog_and_args ~dir prog args =
    Paths (Arg_spec.add_deps args Pset.empty)
    >>>
    (get_prog prog &&&
     (arr (Arg_spec.expand ~dir args)
      >>>
      dyn_paths (arr (fun (_args, deps) -> Path.Set.elements deps))
      >>>
      arr fst))

  let run ?(dir=Path.root) ?stdout_to ?env ?(extra_targets=[]) prog args =
    let extra_targets =
      match stdout_to with
      | None -> extra_targets
      | Some fn -> fn :: extra_targets
    in
    let targets =
      Arg_spec.add_targets args extra_targets
      |> List.map ~f:(fun t -> Target.Normal t)
    in
    prog_and_args ~dir prog args
    >>>
    prim ~targets
      (fun (prog, args) ->
         let stdout_to = Option.map stdout_to ~f:Path.to_string in
         Future.run ~dir:(Path.to_string dir) ?stdout_to ?env
           (Path.reach prog ~from:dir) args)

  let run_capture_gen ~f ?(dir=Path.root) ?env prog args =
    let targets =
      Arg_spec.add_targets args []
      |> List.map ~f:(fun t -> Target.Normal t)
    in
    prog_and_args ~dir prog args
    >>>
    prim ~targets
      (fun (prog, args) ->
         f ?dir:(Some (Path.to_string dir)) ?env (Path.reach prog ~from:dir) args)

  let run_capture ?dir ?env prog args =
    run_capture_gen ~f:Future.run_capture ?dir ?env prog args
  let run_capture_lines ?dir ?env prog args =
    run_capture_gen ~f:Future.run_capture_lines ?dir ?env prog args

  let action ~targets =
    dyn_paths (arr (fun a -> [a.Action.prog]))
    >>>
    prim ~targets:(List.map targets ~f:(fun t -> Target.Normal t))
      (fun { Action. prog; args; env; dir } ->
         Future.run ~dir:(Path.to_string dir) ~env (Path.reach ~from:dir prog) args)

  let echo fn =
    create_file ~target:fn (fun data ->
      with_file_out (Path.to_string fn) ~f:(fun oc -> output_string oc data))

  let deps =
    let rec loop : type a b. (a, b) t -> Pset.t -> Pset.t = fun t acc ->
      match t with
      | Arr _ -> acc
      | Prim _ -> acc
      | Compose (a, b) -> loop a (loop b acc)
      | First t -> loop t acc
      | Second t -> loop t acc
      | Split (a, b) -> loop a (loop b acc)
      | Fanout (a, b) -> loop a (loop b acc)
      | Paths fns -> Pset.union fns acc
      | Vpath (Vspec.T (fn, _)) -> Pset.add fn acc
      | Dyn_paths t -> loop t acc
      | Record_lib_deps _ -> acc
    in
    fun t -> loop t Pset.empty

  let lib_deps =
    let rec loop : type a b. (a, b) t -> String_set.t -> String_set.t = fun t acc ->
      match t with
      | Arr _ -> acc
      | Prim _ -> acc
      | Compose (a, b) -> loop a (loop b acc)
      | First t -> loop t acc
      | Second t -> loop t acc
      | Split (a, b) -> loop a (loop b acc)
      | Fanout (a, b) -> loop a (loop b acc)
      | Paths _ -> acc
      | Vpath _ -> acc
      | Dyn_paths t -> loop t acc
      | Record_lib_deps names -> String_set.union (String_set.of_list names) acc
    in
    fun t -> loop t String_set.empty

  let targets =
    let rec loop : type a b. (a, b) t -> Target.t list -> Target.t list = fun t acc ->
      match t with
      | Arr _ -> acc
      | Prim { targets; _ } -> List.rev_append targets acc
      | Compose (a, b) -> loop a (loop b acc)
      | First t -> loop t acc
      | Second t -> loop t acc
      | Split (a, b) -> loop a (loop b acc)
      | Fanout (a, b) -> loop a (loop b acc)
      | Paths _ -> acc
      | Vpath _ -> acc
      | Dyn_paths t -> loop t acc
      | Record_lib_deps _ -> acc
    in
    fun t -> loop t []

  let exec t x ~targeting =
    let rec exec
      : type a b. (a, b) t -> a -> b Future.t = fun t x ->
      let return = Future.return in
      match t with
      | Arr f -> return (f x)
      | Prim { exec; _ } -> exec x
      | Compose (a, b) ->
        exec a x >>= exec b
      | First t ->
        let x, y = x in
        exec t x >>= fun x ->
        return (x, y)
      | Second t ->
        let x, y = x in
        exec t y >>= fun y ->
        return (x, y)
      | Split (a, b) ->
        let x, y = x in
        both (exec a x) (exec b y)
      | Fanout (a, b) ->
        both (exec a x) (exec b x)
      | Paths _ -> return x
      | Vpath (Vspec.T (fn, kind)) ->
        let file : b File_spec.t = get_file fn (Sexp_file kind) in
        return (Option.value_exn file.data)
      | Dyn_paths t ->
        exec t x >>= fun fns ->
        all_unit (List.rev_map fns ~f:(wait_for_file ~targeting)) >>= fun () ->
        return x
      | Record_lib_deps _ -> return x
    in
    exec t x
end
open Build.O

(* We temporarily allow overrides while setting up copy rules from the source directory so
   that artifact that are already present in the source directory are not re-computed.

   This allows to keep generated files in tarballs. Maybe we should allow it on a
   case-by-case basis though.
*)
let allow_override = ref false
let add_spec fn spec =
  if not !allow_override && Hashtbl.mem files fn then
    die "multiple rules generated for %s" (Path.to_string fn);
  Hashtbl.add files ~key:fn ~data:spec

(*
let target_outside_workspace fn =
  die "target outside source tree: %s" (Path.External.to_string fn)
*)

let create_file_specs targets rule =
  List.iter targets ~f:(function
    | Target.Normal fn ->
      add_spec fn (File_spec.create rule Ignore_contents)
    | Target.Vfile (Vspec.T (fn, kind)) ->
      add_spec fn (File_spec.create rule (Sexp_file kind)))

let no_more_rules_allowed = ref false

let rule dep =
  assert (not !no_more_rules_allowed);
  let fdeps = Build.deps dep in
  let targets = Build.targets dep in
  let ftargets = Target.paths targets in
  let lib_deps = Build.lib_deps dep in
  if !Clflags.debug_rules then begin
    let f set =
      Pset.elements set
      |> List.map ~f:Path.to_string
      |> String.concat ~sep:", "
    in
    if String_set.is_empty lib_deps then
      Printf.eprintf "{%s} -> {%s}\n" (f fdeps) (f ftargets)
    else
      let lib_deps = String_set.elements lib_deps |> String.concat ~sep:", " in
      Printf.eprintf "{%s}, libs:{%s} -> {%s}\n" (f fdeps) lib_deps (f ftargets)
  end;
  add_files all_deps        fdeps;
  add_files buildable_files ftargets;
  let exec = Exec_status.Not_started (fun ~targeting ->
    Pset.iter ftargets ~f:(fun fn ->
      match Path.kind fn with
      | Local local -> Path.Local.ensure_parent_directory_exists local
      | External _ -> ());
    all_unit
      (Pset.fold fdeps ~init:[] ~f:(fun fn acc -> wait_for_file fn ~targeting :: acc))
    >>= fun () ->
    Build.exec dep () ~targeting
  ) in
  let rule =
    { deps = fdeps
    ; targets = ftargets
    ; lib_deps
    ; exec
    } in
  create_file_specs targets rule

let protect_ref r tmp_value ~f =
  protectx !r ~finally:(fun old_v -> r := old_v) ~f:(fun _ ->
    r := tmp_value;
    f ())

let copy_rule ~src ~dst =
  rule
    (Build.path src >>>
     Build.create_file ~target:dst (fun () ->
       copy_file ~src:(Path.to_string src) ~dst:(Path.to_string dst)))

let setup_copy_rules () =
  let contexts = Context.all () in
  protect_ref allow_override true ~f:(fun () ->
    Pset.iter (Pset.union !all_deps !buildable_files) ~f:(fun fn ->
      match Path.extract_build_context fn with
      | Some (name, src) ->
        if String_map.mem name contexts &&
           Path.exists src              &&
           not (Pset.mem src !buildable_files) then
          copy_rule ~src ~dst:fn
      | None ->
        ()
    ))

let remove_old_artifacts () =
  let rec walk dir =
    let keep =
      Path.readdir dir
      |> Array.to_list
      |> List.filter ~f:(fun fn ->
        let fn = Path.relative dir fn in
        if Path.is_directory fn then
          walk fn
        else begin
          let keep = Hashtbl.mem files fn in
          if not keep then Path.unlink fn;
          keep
        end)
      |> function
      | [] -> false
      | _  -> true
    in
    if not keep then Path.rmdir dir;
    keep
  in
  String_map.iter (Context.all ()) ~f:(fun ~key:_ ~data:(ctx : Context.t) ->
    if Path.exists ctx.build_dir then
      ignore (walk ctx.build_dir : bool))

let do_build_exn targets =
  setup_copy_rules ();
  no_more_rules_allowed := true;
  remove_old_artifacts ();
  all_unit (List.map targets ~f:(fun fn -> wait_for_file fn ~targeting:fn))

let do_build targets =
  try
    Ok (do_build_exn targets)
  with Build_error.E e ->
    Error e

let rules_for_files paths =
  List.filter_map paths ~f:(fun path ->
    match Hashtbl.find files path with
    | None -> None
    | Some (File_spec.T { rule; _ }) -> Some (path, rule))

module File_closure =
  Top_closure.Make(Path)
    (struct
      type nonrec t = Path.t * t
      type graph = unit
      let key (path, _) = path
      let deps (_, rule) () = rules_for_files (Pset.elements rule.deps)
    end)

let all_lib_deps targets =
  match File_closure.top_closure () (rules_for_files targets) with
  | Ok l ->
    List.fold_left l ~init:String_set.empty ~f:(fun acc (_, rule) ->
      String_set.union rule.lib_deps acc)
  | Error cycle ->
    die "dependency cycle detected:\n   %s"
      (List.map cycle ~f:(fun (path, _) -> Path.to_string path)
       |> String.concat ~sep:"\n-> ")
