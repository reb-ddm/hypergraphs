open Hypergraph_sig

(**
DATA STRUCTURE:
Hypergraph which remembers for each node a minimal (not necessarily the shortest, but the "first" one that was added) path from the initial states to the node.
ADDING A CLAUSE:
Restart fixpoint iteration.
REMOVING A CLAUSE:
If the predecessor of a node is removed, then set all the successors as unreachable.
The take all the nodes that were thus set as unreachable and recalculate their new predecessor.
Optimization: before removing the reachability, check if there is an alternative path from source to goal with <= n steps. (n = max_depth_local_path_search = 3).
RUNTIME:
For adding a clause, linear in the size of the newly reachable nodes.
For removing a clause, either O(1) if a non-predecessor node is removed, else O(number of successors).
*)

module IncrementalHypergraph2 = struct
  let max_depth_local_path_search = 3

  module AdjacencyList = MutableNodeMap (NodeSet)

  module ReachabilityStatus = MutableNodeMap (struct
    include Int

    let empty () = 1
  end)


  module MinPredsMap = MutableNodeMap (struct
    type t = Node.t option

    let empty () = None
  end)

  type t = {
    succs: AdjacencyList.t;
    preds: AdjacencyList.t;
    min_pred: MinPredsMap.t;
    reachability_status: ReachabilityStatus.t;
  }

  let name = "Incremental 2"

  let init ~nr_vars true_node =
    let reachability_status = ReachabilityStatus.empty nr_vars in
    let () = ReachabilityStatus.add true_node 0 reachability_status in
    {
      succs = AdjacencyList.empty nr_vars;
      preds = AdjacencyList.empty nr_vars;
      min_pred = MinPredsMap.empty nr_vars;
      reachability_status;
    }

  let is_reachable node t =
    0 = ReachabilityStatus.get node t.reachability_status

  let decrement_reachability t node =
    let previous_i = ReachabilityStatus.get node t.reachability_status in
    let () =
      ReachabilityStatus.add node (previous_i - 1) t.reachability_status
    in
    t

  let increment_reachability t node =
    let previous_i = ReachabilityStatus.get node t.reachability_status in
    let () =
      ReachabilityStatus.add node (previous_i + 1) t.reachability_status
    in
    t

  let succs_of t node = AdjacencyList.get node t.succs

  let reachable_vars =
    let fold f a b = NodeSet.fold (fun a b -> f b a) b a in
    Hypergraphs.reachable_vars succs_of is_reachable (Node.Compound 0) fold

  let preds_of t node = AdjacencyList.get node t.preds

  let add_successor node ~succ t =
    let successors = succs_of t node in
    if NodeSet.mem succ successors then
      t
    else (
      let new_set = NodeSet.add succ successors in
      let () = AdjacencyList.add node new_set t.succs in
      t
    )

  let add_predecessor node ~pred t =
    let predecessors = preds_of t node in
    if NodeSet.mem pred predecessors then
      t
    else (
      let new_set = NodeSet.add pred predecessors in
      let () = AdjacencyList.add node new_set t.preds in
      t
    )

  let get_min_pred node t = MinPredsMap.get node t.min_pred

  let add_min_pred node ~min_pred t =
    let () = MinPredsMap.add node (Some min_pred) t.min_pred in
    t

  let remove_min_pred node t =
    let () = MinPredsMap.remove node t.min_pred in
    t

  let add_compound_node node set t =
    match node with
    | Node.Compound _id ->
      if ReachabilityStatus.mem node t.reachability_status then
        t
      else (
        let reach =
          IdSet.fold
            (fun node reach ->
              let status =
                ReachabilityStatus.get (Simple node) t.reachability_status
              in
              reach + status)
            set 0
        in
        let () = ReachabilityStatus.add node reach t.reachability_status in
        t
      )
    | Node.Simple _ -> t

  let remove_compound_node t node =
    match node with
    | Node.Simple _ -> t
    | Node.Compound _ ->
      let () = AdjacencyList.remove node t.succs in
      let () = AdjacencyList.remove node t.preds in
      let () = ReachabilityStatus.remove node t.reachability_status in
      t

  (* ---------- Add edges ---------- *)

  let add_simple_edge q1 q2 hg =
    hg |> add_successor q1 ~succ:q2 |> add_predecessor q2 ~pred:q1

  let add_hyperedge =
    Hypergraphs.add_hyperedge add_compound_node add_simple_edge add_simple_edge
      (fun x -> x)

  let rec closure node ~pred t =
    if is_reachable node t then
      t
    else (
      let t = decrement_reachability t node in
      if is_reachable node t then (
        let t =
          if Node.is_simple node then
            add_min_pred node ~min_pred:pred t
          else
            t
        in
        let succs = succs_of t node in
        NodeSet.fold (fun succ t -> closure succ ~pred:node t) succs t
      ) else
        t
    )

  (* add an edge x -> y to the hypergraph *)
  let add_clause (y, x, x_set) t =
    let t = add_hyperedge x_set x y t in
    if is_reachable x t then
      closure y ~pred:x t
    else
      t

  let add_clauses = Hypergraphs.add_clauses add_clause

  let weight_of t node =
    ReachabilityStatus.get node t.reachability_status
    |> string_of_int |> Option.some

  let remove_successor node ~succ t =
    let successors = succs_of t node in
    if NodeSet.mem succ successors then (
      let new_set = NodeSet.remove succ successors in
      let () = AdjacencyList.add node new_set t.succs in
      t
    ) else
      t

  let remove_predecessor node ~pred t =
    let predecessors = preds_of t node in
    if NodeSet.mem pred predecessors then (
      let new_set = NodeSet.remove pred predecessors in
      let () = AdjacencyList.add node new_set t.preds in
      t
    ) else
      t

  let remove_simple_edge tail head hg =
    hg |> remove_successor tail ~succ:head |> remove_predecessor head ~pred:tail

  let remove_hyperedge =
    Hypergraphs.remove_hyperedge remove_simple_edge NodeSet.is_empty succs_of
      remove_successor remove_compound_node

  let scan_successors_after_removal hg y =
    let visit_node v y hg =
      Node.is_simple v
      &&
      match get_min_pred v hg with
      | None -> false
      | Some min_pred -> Node.compare min_pred y = 0
    in
    let hg, c_set =
      Hypergraphs.update_successors_after_removal is_reachable
        increment_reachability
        (fun hg y ->
          let hg = remove_min_pred y hg in
          increment_reachability hg y)
        visit_node succs_of y [] hg
    in
    Hypergraphs.propagate is_reachable preds_of hg closure c_set

  (*BFS*)
  let search_alternative_local_path x_set y hg =
    let queue = Queue.create () in
    let () = IdSet.iter (fun i -> Queue.push (0, i) queue) x_set in
    let process_simple_node i reachable_nodes depth =
      let reachable_nodes = IdSet.add i reachable_nodes in
      if depth < max_depth_local_path_search then (
        let () = Queue.add (depth + 1, i) queue in
        reachable_nodes
      ) else
        reachable_nodes
    in
    let rec next_edge_of_path reachable_nodes =
      if Queue.is_empty queue then
        None
      else (
        let depth, node_id = Queue.take queue in
        let node = Node.Simple node_id in
        let succs = succs_of hg node in
        if NodeSet.mem y succs then
          Some node
        else (
          let reachable_nodes, result =
            NodeSet.fold
              (fun succ (reachable_nodes, acc) ->
                if Option.is_some acc then
                  reachable_nodes, acc
                else (
                  match succ with
                  | Node.Simple i ->
                    process_simple_node i reachable_nodes depth, acc
                  | Node.Compound _ ->
                    let preds =
                      NodeSet.fold
                        (fun pred idset ->
                          match pred with
                          | Node.Simple pred -> IdSet.add pred idset
                          | Node.Compound _ -> idset)
                        (preds_of hg succ) (IdSet.empty ())
                    in
                    if IdSet.subset preds reachable_nodes then (
                      let succs = succs_of hg succ in
                      if NodeSet.mem y succs then
                        ( reachable_nodes,
                          Some succ (*TODO should stop the iteration instead*) )
                      else
                        ( NodeSet.fold
                            (fun succ2 reachable_nodes ->
                              match succ2 with
                              | Node.Simple i ->
                                process_simple_node i reachable_nodes depth
                              | Node.Compound _ -> reachable_nodes)
                            succs reachable_nodes,
                          acc )
                    ) else
                      reachable_nodes, acc
                ))
              succs (reachable_nodes, None)
          in
          match result with
          | Some _ -> result
          | None -> next_edge_of_path reachable_nodes
        )
      )
    in
    next_edge_of_path x_set

  (* remove a hyperedge x -> y from the graph *)
  let remove_clause (y, x, x_set) hg =
    if Node.is_reflexive_edge x_set y then
      (*reflexive edges are not allowed*)
      hg
    else (
      let hg = remove_hyperedge x_set x y hg in
      match get_min_pred y hg with
      | None -> (* y is unreachable *) hg
      | Some min_pred ->
        if Node.compare min_pred x = 0 then (
          match search_alternative_local_path x_set y hg with
          | Some node -> add_min_pred y ~min_pred:node hg
          | None -> scan_successors_after_removal hg y
        ) else
          hg
    )

  let open_chapter _ t = t
  let close_chapter t = t
end

include IncrementalHypergraph2
