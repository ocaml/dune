open Import

module Pset = Path.Set

module Vspec = struct
  type 'a t = T : Path.t * 'a Vfile_kind.t -> 'a t
end

module Prog_spec = struct
  type 'a t =
    | Dep of Path.t
    | Dyn of ('a -> Path.t)
end

type lib_dep_kind =
  | Optional
  | Required
type lib_deps = lib_dep_kind String_map.t

module Repr = struct
  type ('a, 'b) prim =
    { targets : Path.t list
    ; exec    : 'a -> 'b Future.t
    }
  type ('a, 'b) t =
    | Arr : ('a -> 'b) -> ('a, 'b) t
    | Prim : ('a, 'b) prim -> ('a, 'b) t
    | Store_vfile : 'a Vspec.t -> ('a, unit) t
    | Compose : ('a, 'b) t * ('b, 'c) t -> ('a, 'c) t
    | First : ('a, 'b) t -> ('a * 'c, 'b * 'c) t
    | Second : ('a, 'b) t -> ('c * 'a, 'c * 'b) t
    | Split : ('a, 'b) t * ('c, 'd) t -> ('a * 'c, 'b * 'd) t
    | Fanout : ('a, 'b) t * ('a, 'c) t -> ('a, 'b * 'c) t
    | Paths : Pset.t -> ('a, 'a) t
    | Paths_glob : Path.t * Re.re -> ('a, 'a) t
    | Vpath : 'a Vspec.t -> (unit, 'a) t
    | Dyn_paths : ('a, Path.t list) t -> ('a, 'a) t
    | Record_lib_deps : Path.t * lib_deps -> ('a, 'a) t
    | Fail : fail -> ('a, 'a) t
end
include Repr
let repr t = t

let merge_lib_deps a b =
  String_map.merge a b ~f:(fun _ a b ->
    match a, b with
    | None, None -> None
    | x, None | None, x -> x
    | Some a, Some b ->
      Some (match a, b with
        | Optional, Optional -> Optional
        | _ -> Required))

let arr f = Arr f
let return x = Arr (fun () -> x)

let record_lib_deps ~dir ~kind lib_deps =
  Record_lib_deps
    (dir,
     List.concat_map lib_deps ~f:(function
       | Jbuild_types.Lib_dep.Direct s -> [(s, kind)]
       | Select { choices; _ } ->
         List.concat_map choices ~f:(fun c ->
           List.filter_map c.Jbuild_types.Lib_dep.lits ~f:(function
             | Pos d -> Some (d, Optional)
             | Neg _ -> None)))
     |> String_map.of_alist_exn)

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
let paths_glob ~dir re = Paths_glob (dir, re)
let vpath vp = Vpath vp
let dyn_paths t = Dyn_paths t

let fail x = Fail x

let files_recursively_in ~dir =
  let ctx_dir, src_dir =
    match Path.extract_build_context_dir dir with
    | None -> (Path.root, dir)
    | Some (ctx_dir, src_dir) -> (ctx_dir, src_dir)
  in
  let rec loop dir acc =
    List.fold_left (Path.readdir dir) ~init:acc ~f:(fun acc fn ->
      let path = Path.relative dir fn in
      if Path.is_directory path then
        loop path acc
      else
        Pset.add (Path.append ctx_dir path) acc)
  in
  path_set (loop src_dir Pset.empty)

let prim ~targets exec = Prim { targets; exec }

let create_files ~targets exec =
  prim ~targets (fun x -> Future.return (exec x))
let create_file ~target exec =
  create_files ~targets:[target] exec

let store_vfile spec = Store_vfile spec

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
  let targets = Arg_spec.add_targets args extra_targets in
  prog_and_args ~dir prog args
  >>>
  prim ~targets
    (fun (prog, args) ->
       let stdout_to =
         match stdout_to with
         | None -> Future.Terminal
         | Some path -> File (Path.to_string path)
       in
       Future.run Strict ~dir:(Path.to_string dir) ~stdout_to ?env
         (Path.reach prog ~from:dir) args)

let run_capture_gen ~f ?(dir=Path.root) ?env prog args =
  let targets = Arg_spec.add_targets args [] in
  prog_and_args ~dir prog args
  >>>
  prim ~targets
    (fun (prog, args) ->
       f ?dir:(Some (Path.to_string dir)) ?env
         Future.Strict (Path.reach prog ~from:dir) args)

let run_capture ?dir ?env prog args =
  run_capture_gen ~f:Future.run_capture ?dir ?env prog args
let run_capture_lines ?dir ?env prog args =
  run_capture_gen ~f:Future.run_capture_lines ?dir ?env prog args

module Shexp = struct
  open Future
  open Action.Mini_shexp

  let rec exec t ~dir ~env ~env_extra ~stdout_to ~tail =
    match t with
    | Run (prog, args) ->
      let stdout_to : Future.stdout_to =
        match stdout_to with
        | None          -> Terminal
        | Some (fn, oc) -> Opened_file { filename = fn; tail; desc = Channel oc }
      in
      let env = Context.extend_env ~vars:env_extra ~env in
      Future.run Strict ~dir:(Path.to_string dir) ~env ~stdout_to prog args
    | Chdir (fn, t) ->
      exec t ~env ~env_extra ~stdout_to ~tail ~dir:(Path.relative dir fn)
    | Setenv (var, value, t) ->
      exec t ~dir ~env ~stdout_to ~tail
        ~env_extra:(String_map.add env_extra ~key:var ~data:value)
    | With_stdout_to (fn, t) ->
      if tail then Option.iter stdout_to ~f:(fun (_, oc) -> close_out oc);
      let fn = Path.to_string (Path.relative dir fn) in
      exec t ~dir ~env ~env_extra ~tail
        ~stdout_to:(Some (fn, open_out_bin fn))
    | Progn l ->
      exec_list l ~dir ~env ~env_extra ~stdout_to ~tail
    | Echo str ->
      return
        (match stdout_to with
         | None -> print_string str; flush stdout
         | Some (_, oc) ->
           output_string oc str;
           if tail then close_out oc)
    | Cat fn ->
      let fn = Path.to_string (Path.relative dir fn) in
      with_file_in fn ~f:(fun ic ->
        match stdout_to with
        | None -> copy_channels ic stdout
        | Some (_, oc) ->
          copy_channels ic oc;
          if tail then close_out oc);
      return ()
    | Copy_and_add_line_directive (src, dst) ->
      let src = Path.relative dir src in
      let dst = Path.relative dir dst in
      with_file_in (Path.to_string src) ~f:(fun ic ->
        with_file_out (Path.to_string dst) ~f:(fun oc ->
          let fn =
            match Path.extract_build_context src with
            | None -> src
            | Some (_, rem) -> rem
          in
          Printf.fprintf oc "# 1 %S\n" (Path.to_string fn);
          copy_channels ic oc));
      return ()
    | System cmd ->
      let path, arg, err =
        Utils.system_shell ~needed_to:"interpret (system ...) actions"
      in
      match err with
      | Some err -> err.fail ()
      | None ->
        exec ~dir ~env ~env_extra ~stdout_to ~tail
          (Run (Path.to_string path, [arg; cmd]))

  and exec_list l ~dir ~env ~env_extra ~stdout_to ~tail =
    match l with
    | [] ->
      if tail then Option.iter stdout_to ~f:(fun (_, oc) -> close_out oc);
      Future.return ()
    | [t] ->
      exec t ~dir ~env ~env_extra ~stdout_to ~tail
    | t :: rest ->
      exec t ~dir ~env ~env_extra ~stdout_to ~tail:false >>= fun () ->
      exec_list rest ~dir ~env ~env_extra ~stdout_to ~tail

  let exec t ~dir ~env =
    exec t ~dir ~env ~env_extra:String_map.empty ~stdout_to:None ~tail:true
end

let action ~dir ~env ~targets =
  prim ~targets (fun action ->
    match (action : string Action.t) with
    | Bash cmd ->
      Future.run Strict ~dir:(Path.to_string dir) ~env
        "/bin/bash" ["-e"; "-u"; "-o"; "pipefail"; "-c"; cmd]
    | Shexp shexp ->
      Shexp.exec ~dir ~env shexp)

let echo fn =
  create_file ~target:fn (fun data ->
    with_file_out (Path.to_string fn) ~f:(fun oc -> output_string oc data))

let copy ~src ~dst =
  path src >>>
  create_file ~target:dst (fun () ->
    copy_file ~src:(Path.to_string src) ~dst:(Path.to_string dst))

let touch target =
  create_file ~target (fun _ ->
    Unix.close
      (Unix.openfile (Path.to_string target)
         [O_CREAT; O_TRUNC; O_WRONLY] 0o666))
