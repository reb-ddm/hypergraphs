open Hypergraph_sig

module IncrementalHypergraph  = struct

  module AdjacencyList = MutableNodeMap(NodeSet)
  module ReachabilityStatus = MutableNodeMap(struct include Int let empty = 1 end)
  module NrIncomingEdges = MutableNodeMap(struct include Int let empty = 0 end)

  type t = {
    adjacency_list : AdjacencyList.t;
    reachability_status : ReachabilityStatus.t;
    nr_incoming_edges : NrIncomingEdges.t; (*we only store the number of incoming nodes for the compound nodes, not the simple nodes.*)
  }

  let init ~nr_vars = 
    {
      adjacency_list = AdjacencyList.empty nr_vars;
      reachability_status = 
        ReachabilityStatus.add True 0 (ReachabilityStatus.empty nr_vars);
      nr_incoming_edges = NrIncomingEdges.empty 0;
    }

  let is_reachable node t = 
    0 = ReachabilityStatus.get node t.reachability_status

  let decrement_reachability t node = 
    let previous_i = 
      ReachabilityStatus.get node t.reachability_status in 
    {t with 
     reachability_status =
       ReachabilityStatus.add node (previous_i - 1) t.reachability_status}

  let succs_of t node =
    AdjacencyList.get node t.adjacency_list

  let add_successor node ~succ:succ t = 
    let successors = succs_of t node in 
    if NodeSet.mem succ successors then 
      t
    else
      let new_set = NodeSet.add succ successors in
      let adjacency_list = 
        AdjacencyList.add node new_set t.adjacency_list in
      {t with adjacency_list}

  let add_compound_node node set t =
    match node with
    | Node.Compound id ->
      let reach = IdSet.fold (fun node reach ->
          let status = 
            ReachabilityStatus.get (Simple node) t.reachability_status
          in reach + status
        ) set 0 in
      let reachability_status = 
        ReachabilityStatus.add node reach t.reachability_status
      in
      let nr_nodes = IdSet.cardinal set in
      let nr_incoming_edges = NrIncomingEdges.add (Node.Compound id) nr_nodes t.nr_incoming_edges in
      {t with reachability_status; nr_incoming_edges}
    | Node.Simple _ | Node.True -> t

  let remove_compound_node t node = 
    match node with 
    | Node.True | Node.Simple _ -> t
    | Node.Compound _ -> 
      { adjacency_list = AdjacencyList.remove node t.adjacency_list;
        reachability_status = ReachabilityStatus.remove node t.reachability_status;
        nr_incoming_edges = NrIncomingEdges.remove node t.nr_incoming_edges }

  (* ---------- Add edges ---------- *)

  let add_simple_edge q1 q2 hg =
    hg 
    |> add_successor q1 ~succ:q2 

  let add_hyperedge t tail_set tail head =
    if Node.is_reflexive_edge tail_set head then 
      (*reflexive edges are not allowed*)
      t
    else 
      let t = 
        if Node.is_simple tail then t else 
          (* add nodes from the simple edges to the compound edge "tail" *)
          IdSet.fold (
            fun i acc_hg -> add_simple_edge (Simple i) tail acc_hg
          ) tail_set (add_compound_node tail tail_set t)
      in
      add_simple_edge tail head t

  let rec closure node t =
    if is_reachable node t then
      t 
    else
      let t = decrement_reachability t node in
      if is_reachable node t then 
        let succs = succs_of t node in 
        NodeSet.fold (fun succ t ->
            closure succ t) succs t
      else
        t

  (* add an edge x -> y to the hypergraph *)
  let add_clause (y, x, x_set) t =
    let t = add_hyperedge t x_set x y in
    if is_reachable x t then 
      closure y t 
    else t

  let add_clauses cls hg =
    List.fold_left (fun acc c -> add_clause c acc) hg cls

  let weight_of t node =  
    Some (ReachabilityStatus.get node t.reachability_status)

  let remove_successor node ~succ:succ t = 
    let successors = succs_of t node in 
    if NodeSet.mem succ successors then 
      let new_set = NodeSet.remove succ successors in
      let adjacency_list = 
        AdjacencyList.add node new_set t.adjacency_list in
      {t with adjacency_list}
    else
      t

  let remove_hyperedge tail_set tail head hg =
    let hg = remove_successor tail ~succ:head hg in
    if Node.is_compound tail then
      let hg = 
        IdSet.fold (fun i ->
            remove_successor (Simple i) ~succ:tail
          ) tail_set hg
      in
      if NodeSet.is_empty (succs_of hg tail) then
        remove_compound_node hg tail
      else hg
    else
      remove_successor tail ~succ:head hg

  let remove_all_weights t =
    let reachability_status = ReachabilityStatus.map (fun node _ -> 
        match node with 
        | Node.True -> 1
        | Node.Simple _ -> 1
        | Node.Compound _ -> 
          NrIncomingEdges.get node t.nr_incoming_edges
      ) t.reachability_status
    in 
    {t with reachability_status}

  (** Recalculates all weights from scratch. *)
  let remove_clause (y, x, x_set) hg = 
    if Node.is_reflexive_edge x_set y then 
      (*reflexive edges are not allowed*)
      hg 
    else
      hg |> remove_hyperedge x_set x y 
      |> remove_all_weights (*set all nodes to unreachable (except the node "true")*)
      |> closure True

end

include IncrementalHypergraph