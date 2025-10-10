open Util

(*TODO in this implementation the information pred(c) for a compound node c is redundant as it is already in the set c. We should use an id for the compound nodes instead or remove pred(c)*)
exception UnknownNodeId of (string * Node.t)

module Hypergraph : HornClausesReachability = struct
  module NodeData = struct
    type t = {
      weight : int option; (* rank(/length) of the minimal path from the source (="true" node) to the current node. None if the node is unreachable. *)
      min_pred : Node.t option; (* Previous node in the minimal path from the source. None if the node is compound or if the node is unreachable. *)
      succs : NodeSet.t; (* outgoing edges *)
      preds : NodeSet.t; (* incoming edges *)
    }

    let make_node_data ?(weight=None) ?(min_pred=None) ?(succs=NodeSet.empty) ?(preds=NodeSet.empty) () =
      { weight; min_pred; succs; preds }

    let update_weight nd w = {nd with weight = w}
    let update_min_pred nd minpred = {nd with min_pred = minpred}
    let is_reachable nd = Option.is_some nd.weight
    let add_successor nd succ = {nd with succs = NodeSet.add succ nd.succs}
    let add_predecessor nd pred = {nd with preds = NodeSet.add pred nd.preds}
    let remove_successor nd succ = {nd with succs = NodeSet.remove succ nd.succs}
    let remove_predecessor nd pred = {nd with preds = NodeSet.remove pred nd.preds}

  end 

  type t = NodeData.t NodeMap.t

  module PQ = PriorityQueue(Node)

  let init ~nr_vars = 
    let _ = nr_vars in
    let true_node_data = NodeData.make_node_data ~weight:(Some 0) () in
    let node_map = NodeMap.empty in NodeMap.add True true_node_data node_map

  let get_node_data hg node =
    match NodeMap.find_opt node hg with
    | Some nd -> nd
    | None -> raise (UnknownNodeId (__LOC__, node))

  let set_node_data hg node nd = NodeMap.add node nd hg

  let add_node node hg = 
    if Node.is_compound node then 
      set_node_data hg node (NodeData.make_node_data ~weight:(Some 0) ())
    else
      set_node_data hg node (NodeData.make_node_data ())

  let remove_node hg node = NodeMap.remove node hg

  let weight_of hg node =
    (get_node_data hg node).NodeData.weight

  let min_pred_of hg node =
    (get_node_data hg node).NodeData.min_pred

  let succs_of hg node =
    (get_node_data hg node).NodeData.succs

  let preds_of hg node =
    (get_node_data hg node).NodeData.preds

  let update_min_pred node min_pred w hg =
    if Node.is_compound node then 
      (set_node_data hg node (NodeData.update_weight (get_node_data hg node) w)) 
    else 
      (set_node_data hg node (NodeData.update_weight (NodeData.update_min_pred (get_node_data hg node) (Some min_pred)) w)) 

  let remove_min_pred node hg = 
    (set_node_data hg node (NodeData.update_weight (NodeData.update_min_pred (get_node_data hg node) None) None))

  let is_reachable node hg = NodeData.is_reachable (get_node_data hg node)
  let add_successor node ~succ:succ hg = 
    (set_node_data hg node (NodeData.add_successor (get_node_data hg node) succ))
  let add_predecessor node ~pred:pred hg = 
    (set_node_data hg node (NodeData.add_predecessor (get_node_data hg node) pred))
  let remove_successor node ~succ:succ hg = 
    (set_node_data hg node (NodeData.remove_successor (get_node_data hg node) succ))
  let remove_predecessor node ~pred:pred hg = 
    (set_node_data hg node (NodeData.remove_predecessor (get_node_data hg node) pred))

  (* The weight the next node would have if there was a simple edge from this node to the successor*)
  let weight_of_successor hg node =
    if Node.is_simple node then
      Option.map ((+) 1) (weight_of hg node)
    else 
      weight_of hg node

  (*None represents the largest value (infinite)*)
  let opt_min x y = 
    Option.fold ~none:x ~some:(fun y -> Option.fold ~none:(Some y) ~some:(fun x -> Some (min y x)) x) y

  let opt_max x y = Option.fold ~none:None ~some:(fun y -> Option.map (max y) x) y

  (* ---------- Add edges ---------- *)


  (* this function updates the weight of the node q2 after an insertion of the edge q1 -> q2.
     It returns true if the weight of q2 was changed. *)
  let update_weight_for_new_edge q2 q1 hg =
    let w1_successor = weight_of_successor hg q1 in
    let w_q2 = weight_of hg q2 in
    let new_w =
      if Node.is_compound q2 then
        opt_max w_q2 w1_successor
      else
        (* Q2 is a simple node *)
        opt_min w_q2 w1_successor
    in
    if new_w <> w_q2 then
      let hg = update_min_pred q2 q1 new_w hg in
      (hg, true)
    else (hg, false)

  let recalculate_weight hg q2 = (*TODO there is a better method for this*)
    let hg = if Node.is_simple q2 then
        set_node_data hg q2 (NodeData.update_weight (get_node_data hg q2) None)
      else
        set_node_data hg q2 (NodeData.update_weight (get_node_data hg q2) (Some 0)) in
    let preds = preds_of hg q2 in
    NodeSet.fold (fun q1 acc_hg ->
        let (acc_hg, _ch) = update_weight_for_new_edge q2 q1 acc_hg in
        acc_hg
      ) preds hg

  (* recalculate weight for q2 after the weight of q1 has decreased *)
  let recalculate_weight_decrease q2 q1 hg = (*TODO check*)
    if Node.is_compound q2 then
      let w_q2 = weight_of hg q2 in
      let hg = recalculate_weight hg q2
      in hg, weight_of hg q2 <> w_q2
    else
      (* Q2 is a simple node *)
      update_weight_for_new_edge q2 q1 hg

  (* recalculate weight for q2 after the weight of q1 has increased *)
  let _recalculate_weight_increase q2 q1 hg = (*TODO check*)
    if Node.is_compound q2 then
      update_weight_for_new_edge q2 q1 hg
    else
      (* Q2 is a simple node *)
      let w_q2 = weight_of hg q2 in
      let hg = recalculate_weight hg q2
      in hg, weight_of hg q2 <> w_q2

  let add_simple_edge q1 q2 hg =
    hg 
    |> add_successor q1 ~succ:q2 
    |> add_predecessor q2 ~pred:q1 
    |> update_weight_for_new_edge q2 q1

  let add_hyperedge hg tail head =
    if Node.is_reflexive_edge head tail then 
      (*reflexive edges are not allowed*)
      (hg, false) 
    else 
      let hg = 
        if Node.is_simple tail then hg else 
          (* add nodes from the simple edges to the compound edge "tail" *)
          Node.fold (
            fun i acc_hg -> fst (add_simple_edge i tail acc_hg)
          ) tail (add_node tail hg)
      in
      add_simple_edge tail head hg

  let closure hg start_node =
    let rec propagate hg worklist =
      match PQ.remove_min_opt worklist with
      | None -> hg
      | Some (_, cur, worklist) ->
        let succs = succs_of hg cur in
        let (hg, new_worklist) =
          NodeSet.fold (fun succ (acc_hg, acc_wl) ->
              let (acc_hg, changed) = recalculate_weight_decrease succ cur acc_hg in
              if changed then (acc_hg, PQ.add ~prio:(weight_of hg succ) succ acc_wl) else (acc_hg, acc_wl) (*TODO maybe return the new weight from the function*)
            ) succs (hg, worklist)
        in
        propagate hg new_worklist
    in
    let w = weight_of hg start_node in
    propagate hg (PQ.add ~prio:w start_node PQ.empty)

  (* ---------- Remove edges ---------- *)

  let remove_simple_edge q1 q2 hg =
    hg
    |> remove_successor q1 ~succ:q2 
    |> remove_predecessor q2 ~pred:q1 

  let remove_hyperedge hg tail head =
    if Node.is_compound tail then
      let hg =
        Node.fold (fun i hg_acc ->
            remove_simple_edge i tail hg_acc
          ) tail hg
      in
      let hg = remove_simple_edge tail head hg in
      if NodeSet.is_empty (succs_of hg tail) then
        remove_node hg tail
      else hg
    else
      remove_simple_edge tail head hg

  (*invariant for the priority queue pq: it only contains simple edges *)
  let scan_successors_after_removal hg initial_pq =
    (* select phase *)
    let rec update_successors_after_removal hg pq c_set =
      match PQ.remove_min_opt pq with
      | None -> (hg, c_set)
      | Some (w_y, y, pq) ->
        let pq =
          NodeSet.fold (fun v acc_pq ->
              if Node.is_simple v then
                begin match min_pred_of hg v with
                  | Some pred when Node.compare pred y = 0 ->
                    let w_v = weight_of hg v in (* TODO isnt it weight_of_successor hg pred?*)
                    PQ.add ~prio:w_v v acc_pq
                  | _ -> acc_pq
                end
              else
                NodeSet.fold (fun v' acc_pq ->
                    begin match min_pred_of hg v' with
                      | Some pred when Node.compare pred v = 0 ->
                        PQ.add ~prio:(weight_of hg v') v' acc_pq (*TODO same*)
                      | _ -> acc_pq
                    end
                  ) (succs_of hg v) acc_pq
            ) (succs_of hg y) pq
        in
        (* find hyperedge between the predecessors of y with same weight as minPred(y) *)
        let equivalent =
          NodeSet.find_first_opt (fun pred -> Option.equal Int.equal (weight_of_successor hg pred) w_y) (preds_of hg y)
        in
        match equivalent with 
        | None ->
          (* set weight to infinite if no such edge has been found *)
          let hg = remove_min_pred y hg in
          let hg =
            NodeSet.fold (fun e'' acc_hg ->
                if Node.is_compound e'' then 
                  remove_min_pred e'' acc_hg
                else acc_hg
              ) (succs_of hg y) hg
          in
          update_successors_after_removal hg pq (NodeSet.add y c_set)
        | Some edge ->
          (* else set the edge as the new predecessor in the minimal path *)
          let hg = update_min_pred y edge (weight_of_successor hg edge) hg in
          update_successors_after_removal hg pq c_set   
    in
    let (hg_after_reset, c_set) = 
      update_successors_after_removal hg initial_pq NodeSet.empty 
    in
    (* propagate phase *)
    (* build the priority queue of nodes we need to process *) (*TODO can't you build it instead of building C? Maybe not because the predecessors are changed in the meantime *)
    let pq_s =
      NodeSet.fold (fun z pq_s ->
          (* compute minimal weight among preds *)
          let preds = preds_of hg_after_reset z in
          let rank =
            NodeSet.fold (fun p -> opt_min (weight_of_successor hg_after_reset p)) preds None
          in
          PQ.add ~prio:rank z pq_s
        ) c_set PQ.empty
    in
    (* while S has nodes with rank < infinite, extract min and update the weights *)
    let rec process_s hg s =
      match PQ.remove_min_opt s with
      | None -> hg
      | Some (_w, v, s') ->
        let hg = recalculate_weight hg v in
        let succs = succs_of hg v in
        let (hg, s') =
          NodeSet.fold (fun e (acc_hg, acc_s) ->
              let acc_hg = recalculate_weight acc_hg e in
              if Node.is_simple e then 
                (acc_hg, PQ.decrease_key ~prio:(weight_of hg e) e acc_s)
              else
                (* composite node: update also the head of the edge *)
                NodeSet.fold (fun v' (acc_hg, acc_s) ->
                    let acc_hg = recalculate_weight acc_hg v' in
                    (* update priority of v' in S *)
                    let rank_v' = weight_of hg v' in
                    let acc_s = PQ.decrease_key ~prio:rank_v' v' acc_s
                    in
                    acc_hg, acc_s
                  ) (succs_of acc_hg e) (acc_hg, acc_s)
            ) succs (hg, s')
        in
        process_s hg s'
    in
    process_s hg_after_reset pq_s

  type edge = Node.t * Node.t

  (* add an edge x -> y to the hypergraph *)
  let add_clause (y, x) hg =
    let (hg, changed) = add_hyperedge hg x y in
    if changed then 
      closure hg y
    else hg

  let add_clauses cls hg =
    List.fold_left (fun acc c -> add_clause c acc) hg cls

  (* remove a hyperedge x -> y from the graph *)    
  let remove_clause (y, x) hg =
    let hg = remove_hyperedge hg x y in
    match min_pred_of hg y with
    | Some pred ->
      (* If the removed edge was the predecessor of y in the minimal path from true to y,
         then change the min. predecessor and propagate to all successors of y.*)
      if Node.compare pred x = 0 then
        let w_y = weight_of hg y in
        let pq0 = PQ.add ~prio:w_y y PQ.empty
        in
        scan_successors_after_removal hg pq0
      else hg
    | None -> hg

  (*TODO compile with `dot -Tpng graph.dot -o graph.png`*)
  let to_dot hg int_to_var =
    let b = Buffer.create 1024 in
    Buffer.add_string b "digraph hypergraph {\n";
    Buffer.add_string b "  rankdir=LR;\n";
    Buffer.add_string b "  node [fontsize=10, penwidth=1.2];\n";
    (* Add nodes to buffer *)
    NodeMap.iter (fun node node_data ->
        let w = node_data.NodeData.weight in
        Node.dot_of_node node int_to_var b w
      ) hg;
    (* Add edges to buffer *)
    NodeMap.iter (fun node node_data ->
        NodeSet.iter (fun successor ->
            Node.dot_of_edge node successor b 
          ) (node_data.NodeData.succs)
      ) hg;
    Buffer.add_string b "}\n";
    Buffer.contents b

end



include Hypergraph