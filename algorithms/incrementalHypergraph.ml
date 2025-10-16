open Hypergraph_sig

module IncrementalHypergraph  = struct

  module AdjacencyList = MutableNodeMap(NodeSet)
  module ReachabilityStatus = MutableNodeMap(struct include Int let empty = 1 end)

  type t = {
    adjacency_list : AdjacencyList.t;
    reachability_status : ReachabilityStatus.t;
  }

  let init ~nr_vars = 
    {
      adjacency_list = AdjacencyList.empty nr_vars;
      reachability_status = 
        ReachabilityStatus.add True 0 (ReachabilityStatus.empty nr_vars);
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

  let add_node node set t =
    match node with
    | Node.Compound _ ->
      let reach = IdSet.fold (fun node reach ->
          let status = 
            ReachabilityStatus.get (Simple node) t.reachability_status
          in reach + status
        ) set 0 in
      let reachability_status = 
        ReachabilityStatus.add node reach t.reachability_status
      in
      {t with reachability_status}
    | Node.Simple _ | Node.True -> t

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
          ) tail_set (add_node tail tail_set t)
      in
      add_simple_edge tail head t

  let rec closure t node =
    if is_reachable node t then 
      t 
    else
      let t = decrement_reachability t node in
      if is_reachable node t then 
        let succs = succs_of t node in 
        NodeSet.fold (fun succ t ->
            closure t succ) succs t
      else
        t

  type edge = Node.t * Node.t * IdSet.t

  (* add an edge x -> y to the hypergraph *)
  let add_clause (y, x, x_set) t =
    let t = add_hyperedge t x_set x y in
    if is_reachable x t then 
      closure t y 
    else t

  let add_clauses cls hg =
    List.fold_left (fun acc c -> add_clause c acc) hg cls

  let weight_of t node =  
    Some (ReachabilityStatus.get node t.reachability_status)
    
  let remove_clause = None
end

include IncrementalHypergraph