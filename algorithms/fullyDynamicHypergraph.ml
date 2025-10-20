open Hypergraph_sig

exception UnknownNodeId of (string * Node.t)

module Hypergraph = struct
  module NodeData = struct
    type t = {
      weight : int option; (* rank(/length) of the minimal path from the source (="true" node) to the current node. None if the node is unreachable. *)
      min_pred : Node.t option; (* Previous node in the minimal path from the source. None if the node is compound or if the node is unreachable. *)
      succs : Node.t List.t; (* outgoing edges *)
      preds : Node.t List.t; (* incoming edges *)
    }

    let make_node_data ?(weight=None) ?(min_pred=None) ?(succs=[]) ?(preds=[]) () =
      { weight; min_pred; succs; preds }

    let empty = make_node_data ()

    let update_weight nd w = {nd with weight = w}
    let update_min_pred nd minpred = {nd with min_pred = minpred}
    let is_reachable nd = Option.is_some nd.weight
    let add_successor nd succ = {nd with succs = succ::nd.succs}
    let add_predecessor nd pred = {nd with preds = pred::nd.preds}
    let remove_successor nd succ = {nd with succs = List.filter (fun s -> Node.compare s succ != 0) nd.succs}
    let remove_predecessor nd pred = {nd with preds = List.filter (fun p -> Node.compare p pred != 0) nd.succs}
  end 

  module NodeMap = MutableNodeMap(NodeData)
  type t = NodeMap.t

  module PQ = PriorityQueue(Node)

  let init ~nr_vars = 
    let true_node_data = NodeData.make_node_data ~weight:(Some 0) () in
    NodeMap.add True true_node_data (NodeMap.empty nr_vars)

  let get_node_data hg node = NodeMap.get node hg

  let set_node_data hg node nd = NodeMap.add node nd hg

  let add_compound_node node _set hg = 
    if Node.is_compound node then 
      set_node_data hg node (NodeData.make_node_data ~weight:(Some 0) ())
    else
      set_node_data hg node (NodeData.make_node_data ())

  let remove_compound_node hg node = 
    match node with 
    | Node.True | Node.Simple _ -> hg
    | Node.Compound _ -> NodeMap.remove node hg

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
    set_node_data hg node (NodeData.update_weight (NodeData.update_min_pred (get_node_data hg node) None) None)
  let update_weight node w hg = 
    set_node_data hg node (NodeData.update_weight (get_node_data hg node) w)
  let remove_weight node hg = 
    set_node_data hg node (NodeData.update_weight (get_node_data hg node) None)
  let is_reachable node hg = NodeData.is_reachable (get_node_data hg node)
  let add_successor node ~succ:succ hg = 
    set_node_data hg node (NodeData.add_successor (get_node_data hg node) succ)
  let add_predecessor node ~pred:pred hg = 
    set_node_data hg node (NodeData.add_predecessor (get_node_data hg node) pred)
  let remove_successor node ~succ:succ hg = 
    set_node_data hg node (NodeData.remove_successor (get_node_data hg node) succ)
  let remove_predecessor node ~pred:pred hg = 
    set_node_data hg node (NodeData.remove_predecessor (get_node_data hg node) pred)

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
    if Option.compare Int.compare new_w w_q2 != 0 then
      let hg = update_min_pred q2 q1 new_w hg in
      (hg, true, new_w)
    else (hg, false, new_w)

  let recalculate_weight hg q2 =
    let preds = preds_of hg q2 in
    let weight = 
      if Node.is_compound q2 then
        List.fold_left (fun weight q1 ->
            let w = weight_of_successor hg q1 in
            opt_max weight w
          ) (Some 0) preds
      else 
        List.fold_left (fun weight q1 ->
            let w = weight_of_successor hg q1 in
            opt_min weight w
          ) None preds
    in
    update_weight q2 weight hg, weight


  (* recalculate weight for q2 after the weight of q1 has decreased *)
  let recalculate_weight_decrease q2 q1 hg = 
    if Node.is_compound q2 then
      let w_q2 = weight_of hg q2 in
      let hg, w = recalculate_weight hg q2
      in hg, weight_of hg q2 <> w_q2, w
    else
      (* Q2 is a simple node *)
      update_weight_for_new_edge q2 q1 hg

  (* recalculate weight for q2 after the weight of q1 has increased *)
  let _recalculate_weight_increase q2 q1 hg = 
    if Node.is_compound q2 then
      update_weight_for_new_edge q2 q1 hg
    else
      (* Q2 is a simple node *)
      let w_q2 = weight_of hg q2 in
      let hg, w = recalculate_weight hg q2
      in hg, weight_of hg q2 <> w_q2, w

  let add_simple_edge q1 q2 hg =
    hg 
    |> add_successor q1 ~succ:q2 
    |> add_predecessor q2 ~pred:q1 
    |> update_weight_for_new_edge q2 q1

  let add_hyperedge hg tail_set tail head =
    if Node.is_reflexive_edge tail_set head then 
      (*reflexive edges are not allowed*)
      (hg, false) 
    else 
      let hg = 
        if Node.is_simple tail then hg else 
          (* add nodes from the simple edges to the compound edge "tail" *)
          IdSet.fold (
            fun i acc_hg -> let hg,_,_ = add_simple_edge (Simple i) tail acc_hg in hg
          ) tail_set (add_compound_node tail tail_set hg)
      in
      let hg,b,_ = add_simple_edge tail head hg in hg, b


  let rec propagate hg worklist =
    match PQ.remove_min_opt worklist with
    | None -> hg
    | Some (_, cur, worklist) ->
      let succs = succs_of hg cur in
      let (hg, new_worklist) =
        List.fold_left (fun (acc_hg, acc_wl) succ ->
            let (acc_hg, changed, new_w) = recalculate_weight_decrease succ cur acc_hg in
            if changed then (acc_hg, PQ.add ~prio:new_w succ acc_wl) else (acc_hg, acc_wl)
          ) (hg, worklist) succs
      in
      propagate hg new_worklist

  let closure hg start_node =
    let w = weight_of hg start_node in
    propagate hg (PQ.add ~prio:w start_node PQ.empty)

  (* ---------- Remove edges ---------- *)

  let remove_simple_edge q1 q2 hg =
    hg
    |> remove_successor q1 ~succ:q2 
    |> remove_predecessor q2 ~pred:q1 

  let remove_hyperedge hg tail_set tail head =
    if Node.is_compound tail then
      let hg = 
        hg
        |> IdSet.fold (fun i ->
            remove_successor (Simple i) ~succ:tail
          ) tail_set
        |> remove_simple_edge tail head 
      in
      if (succs_of hg tail) = [] then
        remove_compound_node hg tail
      else hg
    else
      remove_simple_edge tail head hg

  (*invariant for the priority queue pq: it only contains simple edges *)
  let scan_successors_after_removal hg initial_pq =
    (* select phase *)
    let rec update_successors_after_removal pq c_set hg =
      match PQ.remove_min_opt pq with
      | None -> (hg, c_set)
      | Some (w_y, y, pq) ->
        let pq =
          List.fold_left (fun acc_pq v -> (*TODO why add successors to priority queue if the weight might not have changed? Because of loops?*)
              if Node.is_simple v then
                begin match min_pred_of hg v with
                  | Some pred when Node.compare pred y = 0 ->
                    let w_v = weight_of hg v in
                    PQ.add ~prio:w_v v acc_pq
                  | _ -> acc_pq
                end
              else
                List.fold_left (fun acc_pq v' ->
                    begin match min_pred_of hg v' with
                      | Some pred when Node.compare pred v = 0 ->
                        PQ.add ~prio:(weight_of hg v') v' acc_pq
                      | _ -> acc_pq
                    end
                  ) acc_pq (succs_of hg v)
            ) pq (succs_of hg y)
        in
        (* find hyperedge between the predecessors of y with same weight as minPred(y) *)
        let equivalent =
          List.find_opt (fun pred -> Option.equal Int.equal (weight_of_successor hg pred) w_y) (preds_of hg y)
        in
        match equivalent with 
        | None ->
          (* set weight to infinite if no such edge has been found *)
          hg |> remove_min_pred y
          |> fun hg -> List.fold_left (fun hg e'' ->
              if Node.is_compound e'' then 
                remove_weight e'' hg
              else hg
            ) hg (succs_of hg y) 
                       |> update_successors_after_removal pq (y::c_set)
        | Some edge ->
          (* else set the edge as the new predecessor in the minimal path *)
          hg |> update_min_pred y edge (weight_of_successor hg edge)
          |> update_successors_after_removal pq c_set
    in
    let (hg_after_reset, c_set) = 
      update_successors_after_removal initial_pq [] hg
    in
    (* propagate phase *)
    (* build the priority queue of nodes we need to process *)
    let pq_s =
      List.fold_left (fun pq_s z ->
          (* compute minimal weight among preds *)
          let preds = preds_of hg_after_reset z in
          let rank =
            List.fold_left (fun i p -> opt_min (weight_of_successor hg_after_reset p) i) None preds
          in
          PQ.add ~prio:rank z pq_s
        ) PQ.empty c_set
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

  (* add an edge x -> y to the hypergraph *)
  let add_clause (y, x, x_set) hg =
    let (hg, changed) = add_hyperedge hg x_set x y in
    if changed then 
      closure hg y
    else hg

  let add_clauses cls hg =
    List.fold_left (fun acc c -> add_clause c acc) hg cls

  (* remove a hyperedge x -> y from the graph *)    
  let remove_clause (y, x, x_set) hg =
    if Node.is_reflexive_edge x_set y then 
      (*reflexive edges are not allowed*)
      hg 
    else
      let hg = remove_hyperedge hg x_set x y in
      match min_pred_of hg y with
      | Some pred when Node.compare pred x = 0 ->
        (* If the removed edge was the predecessor of y in the minimal path from true to y,
           then change the min. predecessor and propagate to all successors of y.*)
        let w_y = weight_of hg y in
        let pq0 = PQ.add ~prio:w_y y PQ.empty
        in
        scan_successors_after_removal hg pq0
      | Some _ | None -> hg

  let succs_of a b = 
    NodeSet.of_seq (List.to_seq (succs_of a b))
end



include Hypergraph