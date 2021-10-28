  $ cat > dune-project <<EOF
  > (lang dune 2.0)
  > EOF

  $ cat > dune << EOF
  > (rule (target a) (action (bash "echo a > a")))
  > EOF

Graph in GEXF format with actual nodes and edges elided

  $ dune build --dump-memo-graph graph.gexf --dump-memo-graph-format gexf a
  $ cat graph.gexf | grep -v '<node id\|<edge id'
  <?xml version="1.0" encoding="UTF-8"?>
  <gexf xmlns="http://www.gexf.net/1.2draft" version="1.2">
  <graph mode="static" defaultedgetype="directed">
  <nodes>
  </nodes>
  <edges>
  </edges>
  </graph>
  </gexf>

Graph in dot format with actual nodes and edges elided

  $ dune build --dump-memo-graph graph.vg --dump-memo-graph-format dot a
  $ cat graph.vg | grep -v 'n_[0-9]\+ -> n_[0-9]\+'
  strict digraph {
  }
