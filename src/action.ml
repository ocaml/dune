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
      ; cstr "copy" (a @> a @> nil) (fun src dst ->
          With_stdout_to (dst, Cat src))
      ; cstr "copy-and-add-line-directive" (a @> a @> nil) (fun src dst ->
          Copy_and_add_line_directive (src, dst))
      ; cstr "system" (a @> nil) (fun cmd -> System cmd)
      ]
      sexp

  let rec map t ~f =
    match t with
    | Run (prog, args) -> Run (f prog, List.map args ~f)
    | Chdir (fn, t) -> Chdir (f fn, map t ~f)
    | Setenv (var, value, t) -> Setenv (f var, f value, map t ~f)
    | With_stdout_to (fn, t) -> With_stdout_to (f fn, map t ~f)
    | Progn l -> Progn (List.map l ~f:(map ~f))
    | Echo x -> Echo (f x)
    | Cat x -> Cat (f x)
    | Copy_and_add_line_directive (x, y) -> Copy_and_add_line_directive (f x, f y)
    | System x -> System (f x)

  let rec fold t ~init:acc ~f =
    match t with
    | Run (prog, args) -> List.fold_left args ~init:(f acc prog) ~f
    | Chdir (fn, t) -> fold t ~init:(f acc fn) ~f
    | Setenv (var, value, t) -> fold t ~init:(f (f acc var) value) ~f
    | With_stdout_to (fn, t) -> fold t ~init:(f acc fn) ~f
    | Progn l -> List.fold_left l ~init:acc ~f:(fun init t -> fold t ~init ~f)
    | Echo x -> f acc x
    | Cat x -> f acc x
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
    | Copy_and_add_line_directive (x, y) ->
      List [Atom "copy-and-add-line-directive"; f x; f y]
    | System x -> List [Atom "system"; f x]
end

module T = struct
  type 'a t =
    | Bash of 'a
    | Shexp of 'a Mini_shexp.t

  let t a sexp =
    match sexp with
    | Atom _ -> Bash  (a              sexp)
    | List (_, [ Atom (_, "bash"); x ]) -> Bash (a x)
    | List _ -> Shexp (Mini_shexp.t a sexp)

  let map t ~f =
    match t with
    | Bash x -> Bash (f x)
    | Shexp x -> Shexp (Mini_shexp.map x ~f)

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
