open Hypergraph_sig

module IncrementalHypergraph  = struct

  module AdjacencyList = MutableNodeMap(NodeSet)
  module ReachabilityStatus = MutableNodeMap(struct include Int let empty () = 1 end)
  module NrIncomingEdges = MutableNodeMap(struct include Int let empty () = 0 end)

  type t = {
    adjacency_list : AdjacencyList.t;
    reachability_status : ReachabilityStatus.t;
    nr_incoming_edges : NrIncomingEdges.t; (*we only store the number of incoming nodes for the compound nodes, not the simple nodes.*)
    true_node : Node.t
  }

  let init ~nr_vars true_node = 
    let reachability_status = ReachabilityStatus.empty nr_vars in 
    let () = ReachabilityStatus.add true_node 0 reachability_status in
    let nr_incoming_edges = NrIncomingEdges.empty 0 in
    let () = NrIncomingEdges.add true_node 1 nr_incoming_edges in
    {
      adjacency_list = AdjacencyList.empty nr_vars;
      reachability_status;
      nr_incoming_edges;
      true_node;
    }

  let is_reachable node t = 
    0 = ReachabilityStatus.get node t.reachability_status

  let decrement_reachability t node = 
    let previous_i = 
      ReachabilityStatus.get node t.reachability_status in 
    let () = ReachabilityStatus.add node (previous_i - 1) t.reachability_status in t

  let succs_of t node =
    AdjacencyList.get node t.adjacency_list

  let add_successor node ~succ:succ t = 
    let successors = succs_of t node in 
    if NodeSet.mem succ successors then 
      t
    else
      let new_set = NodeSet.add succ successors in
      let () = 
        AdjacencyList.add node new_set t.adjacency_list in
      t

  let add_compound_node node set t =
    match node with
    | Node.Compound id ->
      if ReachabilityStatus.mem node t.reachability_status then t else
        let reach = IdSet.fold (fun node reach ->
            let status = 
              ReachabilityStatus.get (Simple node) t.reachability_status
            in reach + status
          ) set 0 in
        let () = 
          ReachabilityStatus.add node reach t.reachability_status
        in
        let nr_nodes = IdSet.cardinal set in
        let () = NrIncomingEdges.add (Node.Compound id) nr_nodes t.nr_incoming_edges in
        t
    | Node.Simple _ -> t

  let remove_compound_node t (node, _) = 
    match node with 
    | Node.Simple _ -> t
    | Node.Compound _ -> 
      let () = AdjacencyList.remove node t.adjacency_list in
      let () = ReachabilityStatus.remove node t.reachability_status in
      let () = NrIncomingEdges.remove node t.nr_incoming_edges in
      t

  (* ---------- Add edges ---------- *)

  let add_simple_edge q1 q2 hg =
    hg 
    |> add_successor q1 ~succ:q2

  let add_hyperedge =
    Hypergraphs.add_hyperedge add_compound_node add_simple_edge add_simple_edge (fun x -> x)

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
    let t = add_hyperedge x_set x y t in
    if is_reachable x t then 
      closure y t 
    else t

  let add_clauses =
    Hypergraphs.add_clauses add_clause

  let weight_of t node =  
    ReachabilityStatus.get node t.reachability_status |> string_of_int |> Option.some

  let remove_successor node ~succ:succ t = 
    let successors = succs_of t node in 
    if NodeSet.mem succ successors then 
      let new_set = NodeSet.remove succ successors in
      let () = 
        AdjacencyList.add node new_set t.adjacency_list in
      t
    else
      t

  let remove_simple_edge tail head hg = 
    remove_successor tail ~succ:head hg

  let remove_hyperedge =
    Hypergraphs.remove_hyperedge remove_simple_edge NodeSet.is_empty succs_of remove_successor remove_compound_node 

  let remove_all_weights t =
    let reachability_status = ReachabilityStatus.map (fun node _ -> 
        match node with 
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
      |> closure hg.true_node

  let open_chapter _ t = t

end

include IncrementalHypergraph
