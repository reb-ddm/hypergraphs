open Hypergraph_sig

module IncrementalHypergraph2  = struct

  module AdjacencyList = MutableNodeMap(NodeSet)
  module ReachabilityStatus = MutableNodeMap(struct include Int let empty () = 1 end)
  module NrIncomingEdges = MutableNodeMap(struct include Int let empty () = 0 end)

  type t = {
    succs : AdjacencyList.t;
    preds : AdjacencyList.t;
    reachability_status : ReachabilityStatus.t;
  }

  let init ~nr_vars true_node = 
    let reachability_status = ReachabilityStatus.empty nr_vars in 
    let () = ReachabilityStatus.add true_node 0 reachability_status in
    let nr_incoming_edges = NrIncomingEdges.empty 0 in
    let () = NrIncomingEdges.add true_node 1 nr_incoming_edges in
    {
      succs = AdjacencyList.empty nr_vars;
      preds = AdjacencyList.empty nr_vars;
      reachability_status;
    }

  let is_reachable node t = 
    0 = ReachabilityStatus.get node t.reachability_status

  let decrement_reachability t node = 
    let previous_i = 
      ReachabilityStatus.get node t.reachability_status in 
    let () = ReachabilityStatus.add node (previous_i - 1) t.reachability_status in t

  let increment_reachability t node = 
    let previous_i = 
      ReachabilityStatus.get node t.reachability_status in 
    let () = ReachabilityStatus.add node (previous_i + 1) t.reachability_status in t

  let succs_of t node =
    AdjacencyList.get node t.succs

  let preds_of t node =
    AdjacencyList.get node t.preds

  let add_successor node ~succ:succ t = 
    let successors = succs_of t node in 
    if NodeSet.mem succ successors then 
      t
    else
      let new_set = NodeSet.add succ successors in
      let () = 
        AdjacencyList.add node new_set t.succs in
      t

  let add_predecessor node ~pred:pred t = 
    let predecessors = preds_of t node in 
    if NodeSet.mem pred predecessors then 
      t
    else
      let new_set = NodeSet.add pred predecessors in
      let () = 
        AdjacencyList.add node new_set t.preds in
      t

  let add_compound_node node set t =
    match node with
    | Node.Compound _id ->
      if ReachabilityStatus.mem node t.reachability_status then t else
        let reach = IdSet.fold (fun node reach ->
            let status = 
              ReachabilityStatus.get (Simple node) t.reachability_status
            in reach + status
          ) set 0 in
        let () = 
          ReachabilityStatus.add node reach t.reachability_status
        in
        t
    | Node.Simple _ -> t

  let remove_compound_node t (node, _) = 
    match node with 
    | Node.Simple _ -> t
    | Node.Compound _ -> 
      let () = AdjacencyList.remove node t.succs in
      let () = AdjacencyList.remove node t.preds in
      let () = ReachabilityStatus.remove node t.reachability_status in
      t

  (* ---------- Add edges ---------- *)

  let add_simple_edge q1 q2 hg =
    hg 
    |> add_successor q1 ~succ:q2
    |> add_predecessor q2 ~pred:q1

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
        AdjacencyList.add node new_set t.succs in
      t
    else
      t

  let remove_predecessor node ~pred:pred t = 
    let predecessors = preds_of t node in 
    if NodeSet.mem pred predecessors then 
      let new_set = NodeSet.remove pred predecessors in
      let () = 
        AdjacencyList.add node new_set t.preds in
      t
    else
      t

  let remove_simple_edge tail head hg = 
    hg
    |> remove_successor tail ~succ:head
    |> remove_predecessor head ~pred:tail

  let remove_hyperedge =
    Hypergraphs.remove_hyperedge remove_simple_edge NodeSet.is_empty succs_of remove_successor remove_compound_node 

  let scan_successors_after_removal hg y =
    (* select phase *)
    let rec update_successors_after_removal y c_set hg =
      (* visit successors if y is reachable *)
      let was_reachable = is_reachable y hg in
      let hg, c_set = 
        if Node.is_compound y then 
          increment_reachability hg y, c_set else
        if was_reachable then 
          increment_reachability hg y, y::c_set
          (*c_set contains only simple nodes*)
        else 
          hg, c_set in
      let hg, c_set = if was_reachable then
          NodeSet.fold (fun v (hg, c_set) ->
              update_successors_after_removal v c_set hg
            ) (succs_of hg y) (hg, c_set)
        else hg, c_set
      in
      hg, c_set in
    let (hg_after_reset, c_set) = 
      update_successors_after_removal y [] hg
    in
    (* propagate phase *)
    (* build the set of nodes we need to process *)
    List.fold_left (fun hg z ->
        (* if there is a reachable predecessor, then the node is still reachable. *)
        if not (is_reachable z hg) then 
          let preds = preds_of hg_after_reset z in
          let found =
            NodeSet.exists (fun p -> is_reachable p hg) preds
          in
          if found then
            closure z hg
          else 
            hg
        else hg
      ) hg c_set



  (* remove a hyperedge x -> y from the graph *)    
  let remove_clause (y, x, x_set) hg =
    if Node.is_reflexive_edge x_set y then 
      (*reflexive edges are not allowed*)
      hg 
    else
      let hg = remove_hyperedge x_set x y hg in
      scan_successors_after_removal hg y

  let open_chapter _ t = t
  let close_chapter t = t

end

include IncrementalHypergraph2
