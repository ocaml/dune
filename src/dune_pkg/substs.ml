open Import

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
        let name =
          OpamVariable.Full.variable full_variable |> Package_variable.Name.of_opam
        in
        let scope : Package_variable.Scope.t =
          match
            OpamVariable.Full.package ~self:self' full_variable
            |> Option.map ~f:Package_name.of_opam_package_name
          with
          | None -> Self
          | Some p -> Package p
        in
        env { Package_variable.scope; name }
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
