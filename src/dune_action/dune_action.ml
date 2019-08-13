module Protocol = Protocol
module Path = Path
open Protocol

let ( >>> ) f g x = g (f x)

module Fs : sig
  val read_directory : string -> (string list, Unix.error) Stdune.Result.t

  val read_file : string -> (string, Unix.error) Stdune.Result.t

  val write_file : string -> string -> (unit, Unix.error) Stdune.Result.t
end = struct
  let catch_unix_error_exceptions f =
    try Stdune.Ok (f ()) with Unix.Unix_error (e, _, _) -> Stdune.Error e

  let read_directory =
    let rec loop dh acc =
      match Unix.readdir dh with
      | "."
       |".." ->
        loop dh acc
      | s -> loop dh (s :: acc)
      | exception End_of_file -> acc
    in
    fun path ->
      catch_unix_error_exceptions (fun () ->
        let dh = Unix.opendir path in
        Stdune.Exn.protect
          ~f:(fun () -> loop dh [])
          ~finally:(fun () -> Unix.closedir dh))

  let read_file path =
    catch_unix_error_exceptions (fun () ->
      Stdune.Io.String_path.read_file path)

  let write_file path data =
    catch_unix_error_exceptions (fun () ->
      Stdune.Io.String_path.write_file path data)
end

(* TODO jstaron: Store targets to support dynamic target discovery! *)
module Stage = struct
  type 'a t =
    { action : unit -> 'a
    ; dependencies : Dependency.Set.t
    }

  let map (t : 'a t) ~f = { t with action = t.action >>> f }

  let both (t1 : 'a t) (t2 : 'b t) =
    { action = (fun () -> (t1.action (), t2.action ()))
    ; dependencies = Dependency.Set.union t1.dependencies t2.dependencies
    }
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

let rec stage (t : 'a t) ~f =
  match t with
  | Pure a -> f a
  | Stage at -> Stage (Stage.map ~f:(stage ~f) at)

let return a = Pure a

let rec both (t1 : 'a t) (t2 : 'b t) =
  match (t1, t2) with
  | Pure a1, _ -> map ~f:(fun a2 -> (a1, a2)) t2
  | _, Pure a2 -> map ~f:(fun a1 -> (a1, a2)) t1
  | Stage at1, Stage at2 ->
    Stage (Stage.both at1 at2 |> Stage.map ~f:(fun (am1, am2) -> both am1 am2))

let read_file ~path =
  let path = Path.to_string path in
  let action () = Fs.read_file path in
  lift_stage { action; dependencies = Dependency.Set.singleton (File path) }

let write_file ~path ~data =
  let path = Path.to_string path in
  let action () = Fs.write_file path data in
  lift_stage { action; dependencies = Dependency.Set.empty }

let read_directory ~path =
  let path = Path.to_string path in
  let action () = Fs.read_directory path in
  lift_stage
    { action; dependencies = Dependency.Set.singleton (Directory path) }

let rec run_by_dune t context =
  match t with
  | Pure () -> Context.respond context Done
  | Stage at ->
    let provided_dependencies = Context.provided_dependencies context in
    let required_dependencies =
      Dependency.Set.diff at.dependencies provided_dependencies
    in
    if Dependency.Set.is_empty required_dependencies then
      run_by_dune (at.action ()) context
    else
      Context.respond context (Need_more_deps required_dependencies)

(* If executable is not run by dune, assume that all dependencies are provided. *)
let rec run_outside_of_dune t =
  match t with
  | Pure () -> ()
  | Stage at -> run_outside_of_dune (at.action ())

let run t =
  let open Protocol in
  match Context.create ~env_var_name:run_by_dune_env_variable with
  | Run_outside_of_dune -> run_outside_of_dune t
  | Error ->
    failwith
      "Error during communication with dune. Did you use different dune \
       version to compile the executable?"
  | Ok context -> run_by_dune t context
