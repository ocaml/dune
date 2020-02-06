# README for incremental_cycles library

This library is vendored from
https://gitlab.inria.fr/agueneau/incremental-cycles

## Details on the vendoring process

The vendoring process is a bit involved due to the way the library is
specified upstream. In particular, it assumes a graph interface
`Raw_graph` that we have to copy by hand in Dune [see
`src/dag/dag.ml`], and in particular we have to be careful about not
altering the complexity guarantees.


## Complexity guarantees

The complexity and correctness of the implementation of
`incremental_cycles` has been mechanically-verified using the Coq
theorem prover. Note however, that for the main theorem to hold there
are a few requirements that cannot be captured by ML-level interfaces;
more concretely:

- the current specification for the algorithm requires the
  `get_outgoing` function provided by the client to return a list of
  all successors, and do so in constant time. This is quite demanding,
  as basically requires the client to already have the list at hand.

- the main theorem for `Dag.add` does require that the vertex is not
  already in the graph; otherwise the theorem doesn't apply. Thus,
  clients must ensure that no duplicate edge is added to the graph.

## Dune-specific modifications

Dune uses incremental_cycles in a way that the no-duplicate-egdes
requirement is not satisfied by construction; thus, before a call to
`Dag.add` edge membership on the graph must be checked.

This is a common operation and thus should be done efficiently, thus
Dune performs the following modifications to `dag.ml`:

- we add a set of children nodes in addition to the current list
- we modify `raw_add_edge` so it updates this set, and `is_child` so
  it uses the efficient membership set

The rationale for adding a duplicate children field is to actually
preserve the order the edges were added, this could be important in
other parts of the algo, see comment on `is_child` use at `memo.ml`.

For more details see discussion at https://github.com/ocaml/dune/pull/2959
