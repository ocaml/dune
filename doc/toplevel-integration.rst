********************
Toplevel integration
********************

OCaml provides a small repl to use the language interactively. We
generally call this tool a toplevel. The compiler distribution comes
with a small repl called simply ``ocaml`` and the community has
developed enhanced versions such as `utop
<https://github.com/ocaml-community/utop>`_.

It's possible to load dune projects in any toplevel. To do that,
simply execute the following in your toplevel:

.. code:: ocaml

    # #use_output "dune top";;

``dune top`` is a dune command that builds all the libraries in the
current directory and sub-directories and output the relevant toplevel
directives (``#directory`` and ``#load``) to make the various modules
available in the toplevel.

Additionally, if some of the libraries are ppx rewriters the phrases
you type in the toplevel will be rewritten with these ppx rewriters.

This command is available since Dune 2.5.0.

Note that the ``#use_output`` directivce is only available since OCaml
4.11. You can add the following snippet to your ``~/.ocamlinit`` file
to make it available in older versions of OCaml:

.. code:: ocaml

    #directory "+compiler-libs"

    let try_finally ~always f =
      match f () with
      | x ->
        always ();
        x
      | exception e ->
        always ();
        raise e
    
    let use_output command =
      let fn = Filename.temp_file "ocaml" "_toploop.ml" in
      try_finally
        ~always:(fun () -> try Sys.remove fn with Sys_error _ -> ())
        (fun () ->
          match
            Printf.ksprintf Sys.command "%s > %s" command (Filename.quote fn)
          with
          | 0 -> ignore (Toploop.use_file Format.std_formatter fn : bool)
          | n -> Format.printf "Command exited with code %d.@." n)
    
    let () =
      let name = "use_output" in
      if not (Hashtbl.mem Toploop.directive_table name) then
        Hashtbl.add Toploop.directive_table name
          (Toploop.Directive_string use_output)

   ;;
   #remove_directory "+compiler-libs"

