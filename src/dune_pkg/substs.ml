open Import

module Variable = struct
  type t = OpamVariable.t

  let to_dyn v = Dyn.string @@ OpamVariable.to_string v
  let compare a b = Ordering.of_int @@ OpamVariable.compare a b
  let encode v = Dune_lang.atom_or_quoted_string @@ OpamVariable.to_string v
  let of_string = OpamVariable.of_string
end

module Var = struct
  module T = struct
    type t =
      { package : Package_name.t option
      ; variable : Variable.t
      }

    let compare a b =
      match Option.compare Package_name.compare a.package b.package with
      | Eq -> Ordering.of_int @@ OpamVariable.compare a.variable b.variable
      | otherwise -> otherwise
    ;;

    let to_dyn { package; variable } =
      Dyn.pair (Dyn.option Package_name.to_dyn) Variable.to_dyn (package, variable)
    ;;
  end

  include Comparable.Make (T)
  include T
end

module Map = Var.Map

module Make (Monad : sig
    type 'a t

    module O : sig
      val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
    end

    module List : sig
      val map : 'a list -> f:('a -> 'b t) -> 'b list t
    end
  end) =
struct
  open Monad.O

  let is_opam_format src fname =
    let fname = OpamFilename.to_string fname in
    try
      let _ = OpamParser.FullPos.string src fname in
      true
    with
    | _ -> false
  ;;

  let default _ = ""
  let unquoted env write s = write @@ OpamFilter.expand_string ~default env s

  let expand_interpolations_in_opam src env write =
    (* Determine if the input file parses in opam-file-format *)
    let quoted s =
      write
      @@ OpamFilter.expand_string_aux ~escape_value:OpamFilter.escape_value ~default env s
    in
    OpamInterpLexer.main (unquoted env write) quoted (Lexing.from_string src)
  ;;

  let expand_interpolations_line_wise lines env write =
    let rec aux = function
      | [] -> ()
      | line :: lines ->
        unquoted env write line;
        write "\n";
        aux lines
    in
    aux lines
  ;;

  let subst env self ~src ~dst =
    let contents =
      let contents = Io.read_file src in
      let fname = OpamFilename.of_string (Path.to_string src) in
      if is_opam_format contents fname
      then `Opam contents
      else `Lines (String.split_lines contents)
    in
    let expand =
      match contents with
      | `Opam contents -> expand_interpolations_in_opam contents
      | `Lines lines -> expand_interpolations_line_wise lines
    in
    let variables =
      let write _ = () in
      let variables = ref OpamVariable.Full.Set.empty in
      let env var =
        variables := OpamVariable.Full.Set.add var !variables;
        None
      in
      expand env write;
      !variables
    in
    let env =
      let self' = self |> Package_name.to_string |> OpamPackage.Name.of_string in
      fun full_variable ->
        let variable = OpamVariable.Full.variable full_variable in
        let package =
          OpamVariable.Full.package ~self:self' full_variable
          |> Option.map ~f:Package_name.of_opam_package_name
        in
        env { Var.package; variable }
    in
    let+ expansions =
      let+ expanded =
        OpamVariable.Full.Set.to_list_map Fun.id variables
        |> Monad.List.map ~f:(fun var ->
          let+ value = env var in
          var, value)
      in
      OpamVariable.Full.Map.of_list expanded
    in
    let env var = OpamVariable.Full.Map.find var expansions in
    Io.with_file_out (Path.build dst) ~f:(fun oc -> expand env (output_string oc))
  ;;
end
