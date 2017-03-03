open Import
open Sexp.Of_sexp

module Mini_shexp = struct
  type 'a t =
    | Run            of 'a * 'a list
    | Chdir          of 'a * 'a t
    | Setenv         of 'a * 'a * 'a t
    | With_stdout_to of 'a * 'a t
    | Progn          of 'a t list
    | Echo           of 'a
    | Cat            of 'a
    | Copy           of 'a * 'a
    | Symlink        of 'a * 'a
    | Copy_and_add_line_directive of 'a * 'a
    | System         of 'a

  let rec t a sexp =
    sum
      [ cstr_rest "run" (a @> nil) a             (fun prog args -> Run (prog, args))
      ; cstr "chdir"    (a @> t a @> nil)        (fun dn t -> Chdir (dn, t))
      ; cstr "setenv"   (a @> a @> t a @> nil)   (fun k v t -> Setenv (k, v, t))
      ; cstr "with-stdout-to" (a @> t a @> nil)  (fun fn t -> With_stdout_to (fn, t))
      ; cstr_rest "progn"      nil (t a)         (fun l -> Progn l)
      ; cstr "echo"           (a @> nil)         (fun x -> Echo x)
      ; cstr "cat"            (a @> nil)         (fun x -> Cat x)
      ; cstr "copy" (a @> a @> nil)              (fun src dst -> Copy (src, dst))
      (*
         (* We don't expose symlink to the user yet since this might complicate things *)
         ; cstr "symlink" (a @> a @> nil) (fun src dst -> Symlink (dst, Cat src))
      *)
      ; cstr "copy-and-add-line-directive" (a @> a @> nil) (fun src dst ->
          Copy_and_add_line_directive (src, dst))
      ; cstr "system" (a @> nil) (fun cmd -> System cmd)
      ]
      sexp

  let rec expand dir t ~f =
    match t with
    | Run (prog, args) ->
      Run (f dir prog,
           List.map args ~f:(fun arg -> f dir arg))
    | Chdir (fn, t) ->
      let fn = f dir fn in
      Chdir (fn, expand (Path.relative dir fn) t ~f)
    | Setenv (var, value, t) ->
      Setenv (f dir var, f dir value, expand dir t ~f)
    | With_stdout_to (fn, t) ->
      With_stdout_to (f dir fn, expand dir t ~f)
    | Progn l -> Progn (List.map l ~f:(fun t -> expand dir t ~f))
    | Echo x -> Echo (f dir x)
    | Cat x -> Cat (f dir x)
    | Copy (x, y) ->
      Copy (f dir x, f dir y)
    | Symlink (x, y) ->
      Symlink (f dir x, f dir y)
    | Copy_and_add_line_directive (x, y) ->
      Copy_and_add_line_directive (f dir x, f dir y)
    | System x -> System (f dir x)

  let rec fold t ~init:acc ~f =
    match t with
    | Run (prog, args) -> List.fold_left args ~init:(f acc prog) ~f
    | Chdir (fn, t) -> fold t ~init:(f acc fn) ~f
    | Setenv (var, value, t) -> fold t ~init:(f (f acc var) value) ~f
    | With_stdout_to (fn, t) -> fold t ~init:(f acc fn) ~f
    | Progn l -> List.fold_left l ~init:acc ~f:(fun init t -> fold t ~init ~f)
    | Echo x -> f acc x
    | Cat x -> f acc x
    | Copy (x, y) -> f (f acc x) y
    | Symlink (x, y) -> f (f acc x) y
    | Copy_and_add_line_directive (x, y) -> f (f acc x) y
    | System x -> f acc x

  let rec sexp_of_t f : _ -> Sexp.t = function
    | Run (a, xs) -> List (Atom "run" :: f a :: List.map xs ~f)
    | Chdir (a, r) -> List [Atom "chdir" ; f a ; sexp_of_t f r]
    | Setenv (k, v, r) -> List [Atom "setenv" ; f k ; f v ; sexp_of_t f r]
    | With_stdout_to (fn, r) -> List [Atom "with-stdout-to"; f fn; sexp_of_t f r]
    | Progn l -> List (Atom "progn" :: List.map l ~f:(sexp_of_t f))
    | Echo x -> List [Atom "echo"; f x]
    | Cat x -> List [Atom "cat"; f x]
    | Copy (x, y) ->
      List [Atom "copy"; f x; f y]
    | Symlink (x, y) ->
      List [Atom "symlink"; f x; f y]
    | Copy_and_add_line_directive (x, y) ->
      List [Atom "copy-and-add-line-directive"; f x; f y]
    | System x -> List [Atom "system"; f x]
end

module Desc = struct
  module T = struct
    type 'a t =
      | Bash of 'a
      | Shexp of 'a Mini_shexp.t

    let t a sexp =
      match sexp with
      | Atom _ -> Bash  (a              sexp)
      | List _ -> Shexp (Mini_shexp.t a sexp)

    type context = Path.t

    let expand dir t ~f =
      match t with
      | Bash x -> Bash (f dir x)
      | Shexp x -> Shexp (Mini_shexp.expand dir x ~f)

    let fold t ~init ~f =
      match t with
      | Bash x -> f init x
      | Shexp x -> Mini_shexp.fold x ~init ~f

    let sexp_of_t f : _ -> Sexp.t = function
      | Bash a -> List [Atom "bash" ; f a]
      | Shexp a -> List [Atom "shexp" ; Mini_shexp.sexp_of_t f a]
  end

  include T

  module Unexpanded = String_with_vars.Lift(T)
end

type t =
  { env    : string array
  ; dir    : Path.t
  ; action : string Desc.t
  }

let sexp_of_t { env; dir; action } =
  let open Sexp.To_sexp in
  Sexp.List
    [ List [ Atom "env"   ; array string env             ]
    ; List [ Atom "dir"   ; string (Path.to_string dir)  ]
    ; List [ Atom "action"; Desc.sexp_of_t string action ]
    ]
