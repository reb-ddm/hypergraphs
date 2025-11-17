open Hypergraph_sig
 let add_hyperedge add_compound_node add_simple_edge add_simple_edge2 nop tail_set tail head hg =
    if Node.is_reflexive_edge tail_set head then 
      (*reflexive edges are not allowed*)
      nop hg
    else 
      let hg = 
        if Node.is_simple tail then hg else 
          (* add nodes from the simple edges to the compound edge "tail" *)
           hg |> add_compound_node tail tail_set
          |> IdSet.fold (
            fun i hg -> add_simple_edge (Node.Simple i) tail hg
          ) tail_set
      in
      add_simple_edge2 tail head hg

let add_clauses add_clause cls hg =
    List.fold_left (fun acc c -> add_clause c acc) hg cls

let remove_hyperedge remove_simple_edge is_empty succs_of remove_successor remove_compound_node tail_set tail head hg =
  let hg = remove_simple_edge tail head hg in
    if Node.is_compound tail && not (Node.is_true tail) && is_empty (succs_of hg tail) then
        let hg = 
          IdSet.fold (fun i ->
              remove_successor (Node.Simple i) ~succ:tail
            ) tail_set hg
        in
        remove_compound_node hg tail
      else hg