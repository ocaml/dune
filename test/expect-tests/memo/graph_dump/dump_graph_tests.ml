open Stdune
open Memo.O
module Graph = Dune_graph.Graph

module Scheduler = struct
  let t = Test_scheduler.create ()
  let yield () = Test_scheduler.yield t
  let run f = Test_scheduler.run t f
end

(* to run a computation *)
let run m = Scheduler.run (Memo.run m)

let run_memo f v =
  try run (Memo.exec f v) with
  | Memo.Error.E _ -> ()
;;

let a = Memo.create "A" ~input:(module Unit) (fun () -> Memo.return ())

let b =
  Memo.create
    "B"
    ~input:(module Unit)
    (fun () ->
      let+ () = Memo.exec a () in
      ())
;;

let c =
  Memo.create
    "C"
    ~input:(module Unit)
    (fun () ->
      let+ () = Memo.exec a () in
      ())
;;

let d =
  Memo.create
    "D"
    ~input:(module Unit)
    (fun () ->
      let* () = Memo.exec b () in
      let+ () = Memo.exec c () in
      ())
;;

let e =
  Memo.create
    "E"
    ~input:(module Unit)
    (fun () ->
      let* () = Memo.exec d () in
      failwith "Oops, error!")
;;

let () = run_memo e ()

let%expect_test _ =
  let graph = Scheduler.run (Memo.dump_cached_graph (Memo.cell d ())) in
  Graph.print graph ~format:Graph.File_format.Gexf;
  [%expect
    {|
    <?xml version="1.0" encoding="UTF-8"?>
    <gexf xmlns="http://www.gexf.net/1.2draft" version="1.2">
    <graph mode="static" defaultedgetype="directed">
    <nodes>
    <node id="1" label="D" />
    <node id="2" label="B" />
    <node id="3" label="A" />
    <node id="4" label="C" />
    </nodes>
    <edges>
    <edge id="0" source="1" target="2" />
    <edge id="1" source="1" target="4" />
    <edge id="2" source="2" target="3" />
    <edge id="3" source="4" target="3" />
    </edges>
    </graph>
    </gexf> |}]
;;

let%expect_test _ =
  let graph = Scheduler.run (Memo.dump_cached_graph (Memo.cell d ())) in
  Graph.print graph ~format:Graph.File_format.Dot;
  [%expect
    {|
    strict digraph {
    n_1 -> n_2
    n_1 -> n_4
    n_2 -> n_3
    n_4 -> n_3
    } |}]
;;

let%expect_test _ =
  let graph = Scheduler.run (Memo.dump_cached_graph ~time_nodes:true (Memo.cell d ())) in
  Graph.For_tests.print
    graph
    ~format:Graph.File_format.Gexf
    ~opaque_attributes:(Int.Set.singleton 0);
  [%expect
    {|
    <?xml version="1.0" encoding="UTF-8"?>
    <gexf xmlns="http://www.gexf.net/1.2draft" version="1.2">
    <graph mode="static" defaultedgetype="directed">
    <nodes>
    <attributes class="node">
    <attribute id="0" title="runtime" type="float" />
    </attributes>
    <node id="1" label="D">
    <attvalues>
    <attvalue for="0" value="<opaque>" />
    </attvalues>
    </node>
    <node id="2" label="B">
    <attvalues>
    <attvalue for="0" value="<opaque>" />
    </attvalues>
    </node>
    <node id="3" label="A">
    <attvalues>
    <attvalue for="0" value="<opaque>" />
    </attvalues>
    </node>
    <node id="4" label="C">
    <attvalues>
    <attvalue for="0" value="<opaque>" />
    </attvalues>
    </node>
    </nodes>
    <edges>
    <edge id="0" source="1" target="2" />
    <edge id="1" source="1" target="4" />
    <edge id="2" source="2" target="3" />
    <edge id="3" source="4" target="3" />
    </edges>
    </graph>
    </gexf> |}]
;;

let%expect_test _ =
  let graph = Scheduler.run (Memo.dump_cached_graph ~time_nodes:true (Memo.cell e ())) in
  Graph.For_tests.print
    graph
    ~format:Graph.File_format.Gexf
    ~opaque_attributes:(Int.Set.singleton 0);
  [%expect
    {|
    <?xml version="1.0" encoding="UTF-8"?>
    <gexf xmlns="http://www.gexf.net/1.2draft" version="1.2">
    <graph mode="static" defaultedgetype="directed">
    <nodes>
    <attributes class="node">
    <attribute id="0" title="runtime" type="float" />
    </attributes>
    <node id="0" label="E">
    <attvalues>
    <attvalue for="0" value="<opaque>" />
    </attvalues>
    </node>
    <node id="1" label="D">
    <attvalues>
    <attvalue for="0" value="<opaque>" />
    </attvalues>
    </node>
    <node id="2" label="B">
    <attvalues>
    <attvalue for="0" value="<opaque>" />
    </attvalues>
    </node>
    <node id="3" label="A">
    <attvalues>
    <attvalue for="0" value="<opaque>" />
    </attvalues>
    </node>
    <node id="4" label="C">
    <attvalues>
    <attvalue for="0" value="<opaque>" />
    </attvalues>
    </node>
    </nodes>
    <edges>
    <edge id="0" source="0" target="1" />
    <edge id="1" source="1" target="2" />
    <edge id="2" source="1" target="4" />
    <edge id="3" source="2" target="3" />
    <edge id="4" source="4" target="3" />
    </edges>
    </graph>
    </gexf> |}]
;;
