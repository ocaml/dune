open Import

module V1 = struct
  module Path = Path
  module Glob = Dune_glob.V1
  open Protocol

  module Execution_error = struct
    exception E of string

    let raise string = raise (E string)

    let raise_on_fs_error = function
      | Error message -> raise message
      | Ok result -> result
    ;;
  end

  module Directory = struct
    type entry =
      | File of string
      | Directory of t

    and t = entry String.Map.t

    let iter t ~f = String.Map.iteri t ~f

    module Builder = struct
      type empty
      type nonempty
      type 'state builder = t

      let empty = String.Map.empty

      let add_entry t ~name entry =
        if
          String.is_empty name
          || Fpath.contains_path_sep name
          || name = "."
          || name = ".."
        then invalid_arg "Directory entry must be a single path component";
        match String.Map.add t name entry with
        | Ok t -> t
        | Error _ -> invalid_arg (Printf.sprintf "duplicate directory entry %s" name)
      ;;

      let add_file t ~name ~data = add_entry t ~name (File data)
      let add_directory t ~name ~directory = add_entry t ~name (Directory directory)
      let build t = t
    end
  end

  module Fs : sig
    val read_directory : string -> (string list, string) result
    val read_file : string -> (string, string) result
    val write_directory : string -> Directory.t -> (unit, string) result
    val write_file : string -> string -> (unit, string) result
  end = struct
    let catch_system_exceptions f ~name =
      try Ok (f ()) with
      | Unix.Unix_error (error, syscall, arg) ->
        let error = Unix_error.Detailed.create error ~syscall ~arg in
        Error (name ^ ": " ^ Unix_error.Detailed.to_string_hum error)
      | Sys_error error -> Error (name ^ ": " ^ error)
    ;;

    let read_directory =
      let rec loop dh acc =
        match Unix.readdir dh with
        | "." | ".." -> loop dh acc
        | s -> loop dh (s :: acc)
        | exception End_of_file -> acc
      in
      fun path ->
        catch_system_exceptions ~name:"read_directory" (fun () ->
          match Unix.opendir path with
          | exception Unix.Unix_error ((ENOENT | ENOTDIR), _, _) -> []
          | dh ->
            Exn.protect
              ~f:(fun () -> loop dh [] |> List.sort ~compare:String.compare)
              ~finally:(fun () -> Unix.closedir dh))
    ;;

    let read_file path =
      catch_system_exceptions ~name:"read_file" (fun () -> Io.String_path.read_file path)
    ;;

    let write_file path data =
      catch_system_exceptions ~name:"write_file" (fun () ->
        Io.String_path.write_file path data)
    ;;

    let mkdir_p path =
      match Fpath.mkdir_p_strict path with
      | `Created | `Already_exists -> ()
      | `Not_a_dir ->
        raise (Sys_error (Printf.sprintf "%s exists but is not a directory" path))
    ;;

    let write_directory path directory =
      catch_system_exceptions ~name:"write_directory" (fun () ->
        let rec write path directory =
          mkdir_p path;
          Directory.iter directory ~f:(fun name entry ->
            let path = Filename.concat path name in
            match entry with
            | Directory.File data -> Io.String_path.write_file path data
            | Directory.Directory directory -> write path directory)
        in
        write path directory)
    ;;
  end

  module Stage = struct
    type 'a t =
      { action : unit -> 'a
      ; dependencies : Dependency.Set.t
      ; targets : Target.Set.t
      }

    let map (t : 'a t) ~f = { t with action = (fun () -> f (t.action ())) }

    let both (t1 : 'a t) (t2 : 'b t) =
      { action = (fun () -> t1.action (), t2.action ())
      ; dependencies = Dependency.Set.union t1.dependencies t2.dependencies
      ; targets = Target.Set.union t1.targets t2.targets
      }
    ;;
  end

  (* Construction inspired by free monad. *)
  type 'a t =
    | Pure of 'a
    | Stage of 'a t Stage.t

  let lift_stage stage = Stage (Stage.map stage ~f:(fun a -> Pure a))

  let rec map (t : 'a t) ~f =
    match t with
    | Pure a -> Pure (f a)
    | Stage at -> Stage (Stage.map ~f:(map ~f) at)
  ;;

  let rec stage (t : 'a t) ~f =
    match t with
    | Pure a -> f a
    | Stage at -> Stage (Stage.map ~f:(stage ~f) at)
  ;;

  let return a = Pure a

  let rec both (t1 : 'a t) (t2 : 'b t) =
    match t1, t2 with
    | Pure a1, _ -> map ~f:(fun a2 -> a1, a2) t2
    | _, Pure a2 -> map ~f:(fun a1 -> a1, a2) t1
    | Stage at1, Stage at2 ->
      Stage (Stage.both at1 at2 |> Stage.map ~f:(fun (am1, am2) -> both am1 am2))
  ;;

  let read_file ~path =
    let path = Path.to_string path in
    let action () = Fs.read_file path |> Execution_error.raise_on_fs_error in
    lift_stage
      { action
      ; dependencies = Dependency.Set.singleton (File path)
      ; targets = Target.Set.empty
      }
  ;;

  let write_file ~path ~data =
    let path = Path.to_string path in
    let action () = Fs.write_file path data |> Execution_error.raise_on_fs_error in
    lift_stage
      { action
      ; dependencies = Dependency.Set.empty
      ; targets = Target.Set.singleton (Target.File path)
      }
  ;;

  let write_directory ~path ~directory =
    let path = Path.to_string path in
    let action () =
      Fs.write_directory path directory |> Execution_error.raise_on_fs_error
    in
    lift_stage
      { action
      ; dependencies = Dependency.Set.empty
      ; targets = Target.Set.singleton (Target.Directory path)
      }
  ;;

  let read_directory_with_glob ~path ~glob =
    let path = Path.to_string path in
    let action () =
      Fs.read_directory path
      |> Execution_error.raise_on_fs_error
      |> List.filter ~f:(Glob.test glob)
    in
    lift_stage
      { action
      ; dependencies =
          Dependency.Set.singleton (Glob { path; glob = Glob.to_string glob })
      ; targets = Target.Set.empty
      }
  ;;

  let rec run_by_dune t context =
    let is_descendant path ~of_ =
      match String.drop_prefix path ~prefix:(of_ ^ Filename.dir_sep) with
      | None -> false
      | Some path ->
        (match
           Result.try_with (fun () ->
             Stdune.Path.Local.relative Stdune.Path.Local.root path)
         with
         | Ok _ -> true
         | Error _ -> false)
    in
    match t with
    | Pure () -> Context.respond context Done
    | Stage at ->
      let allowed_targets = Context.targets context in
      let target_is_allowed ~declared ~produced =
        match declared, produced with
        | Target.File declared, Target.File produced -> String.equal declared produced
        | File _, Directory _ -> false
        | Directory declared, (File produced | Directory produced) ->
          String.equal declared produced || is_descendant produced ~of_:declared
      in
      let is_allowed produced =
        Target.Set.exists allowed_targets ~f:(fun declared ->
          target_is_allowed ~declared ~produced)
      in
      let disallowed_targets =
        Target.Set.filter at.targets ~f:(fun produced -> not (is_allowed produced))
      in
      let kind_mismatch = function
        | Target.File path ->
          if Target.Set.mem allowed_targets (Directory path)
          then Some (path, "file", "directory")
          else None
        | Directory path ->
          if Target.Set.mem allowed_targets (File path)
          then Some (path, "directory", "file")
          else None
      in
      (match Target.Set.to_list disallowed_targets with
       | [] -> ()
       | [ target ] ->
         let message =
           match kind_mismatch target with
           | Some (path, produced_kind, declared_kind) ->
             Printf.sprintf
               "The %s target %S was produced, but %S is declared as a %s target in the \
                dune file."
               produced_kind
               path
               path
               declared_kind
           | None ->
             Printf.sprintf
               "The %s was produced despite not being declared in the dune file. To fix \
                this, declare it as a target."
               (Target.describe target)
         in
         Execution_error.raise message
       | targets ->
         Execution_error.raise
           (Printf.sprintf
              "The following targets were produced despite not being declared in the \
               dune file:\n\
               %s\n\
               To fix this, declare them as targets."
              (targets
               |> List.map ~f:(fun target -> "- " ^ Target.describe target)
               |> String.concat ~sep:"\n")));
      let prepared_dependencies = Context.prepared_dependencies context in
      let required_dependencies =
        Dependency.Set.diff at.dependencies prepared_dependencies
      in
      if Dependency.Set.is_empty required_dependencies
      then run_by_dune (at.action ()) context
      else Context.respond context (Need_more_deps required_dependencies)
  ;;

  (* If executable is not run by dune, assume that all dependencies are already
     prepared and no target checking is done. *)
  let rec run_outside_of_dune t =
    match t with
    | Pure () -> ()
    | Stage at -> run_outside_of_dune (at.action ())
  ;;

  let do_run t =
    match Protocol.Context.create () with
    | Run_outside_of_dune -> run_outside_of_dune t
    | Error message ->
      Execution_error.raise
        (Printf.sprintf
           "Error during communication with dune. %s Did you use different dune version \
            to compile the executable?"
           message)
    | Ok context -> run_by_dune t context
  ;;

  let run t =
    try
      do_run t;
      exit 0
    with
    | Execution_error.E message ->
      prerr_endline message;
      exit 1
  ;;

  module O = struct
    let ( let+ ) at f = map at ~f
    let ( and+ ) = both
  end

  module Private = struct
    module Protocol = Protocol

    let do_run = do_run

    module Execution_error = Execution_error
  end
end

module Private = V1.Private
