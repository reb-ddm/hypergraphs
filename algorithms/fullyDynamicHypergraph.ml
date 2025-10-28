open Hypergraph_sig

exception UnknownNodeId of (string * Node.t)

module Hypergraph = struct
  module NodeData = struct
    type t = {
      weight : int option; (* rank(/length) of the minimal path from the source (="true" node) to the current node. None if the node is unreachable. *)
      min_pred : Node.t option; (* Previous node in the minimal path from the source. None if the node is compound or if the node is unreachable. *)
      min_succs : NodeSet.t; (* contains all simple nodes for which this node is the min_pred and also contains all the successors that are compound nodes *)
      succs : Node.t List.t; (* outgoing edges *)
      preds : Node.t List.t; (* incoming edges *)
    }

    let make_node_data ?(weight=None) ?(min_pred=None) ?(min_succs=NodeSet.empty ()) ?(succs=[]) ?(preds=[]) () =
      { weight; min_pred; min_succs; succs; preds }

    let empty () = make_node_data ()

    let update_weight w nd = {nd with weight = w}
    let update_min_pred minpred nd = {nd with min_pred = minpred}
    let is_reachable nd = Option.is_some nd.weight
    let add_min_succ succ nd = {nd with min_succs = NodeSet.add succ nd.min_succs}
    let add_successor succ nd = {nd with succs = succ::nd.succs}
    let add_predecessor pred nd = {nd with preds = pred::nd.preds}
    let remove_successor succ nd = {nd with succs = List.filter (fun s -> Node.compare s succ != 0) nd.succs}
    let remove_predecessor pred nd = {nd with preds = List.filter (fun p -> Node.compare p pred != 0) nd.succs}
    let remove_min_succ succ nd = {nd with min_succs = NodeSet.remove succ nd.min_succs}

  end

  module PsqPriorityQueue(E: OrderedAndPrintable) = struct
    module IntOption = struct
      type t = int option
      let compare x y = match x,y with
        | None, None -> 0
        | None, Some _ -> 1
        | Some _, None -> -1
        | Some x, Some y -> Int.compare x y
    end
    include Psq.Make(E)(IntOption)
    let remove_min_opt q = match pop q with 
      | Some ((a,b),c) -> Some (b,a,c)
      | None -> None 
    let add ~prio elt q = add elt prio q

    let decrease_key ~prio elt q = 
      push elt prio q
  end

  module NodeMap = MutableNodeMap(NodeData)
  type t = NodeMap.t

  module PQ = PsqPriorityQueue(Node)

  let init ~nr_vars true_node = 
    let true_node_data = NodeData.make_node_data ~weight:(Some 1) () in
    let node_map = NodeMap.empty nr_vars in
    let () = NodeMap.add true_node true_node_data node_map in
    node_map

  let get_node_data hg node = NodeMap.get node hg

  let set_node_data hg node nd = NodeMap.add node nd hg; hg

  let add_compound_node node _set hg = 
    if NodeMap.mem node hg then hg else
    if Node.is_compound node then 
      NodeData.make_node_data ~weight:(Some 0) () |> set_node_data hg node
    else
      NodeData.make_node_data () |> set_node_data hg node

  let remove_compound_node hg node = 
    match node with 
    | Node.Simple _ -> hg
    | Node.Compound _ -> let () = NodeMap.remove node hg in hg

  let weight_of hg node =
    (get_node_data hg node).NodeData.weight

  let min_pred_of hg node =
    (get_node_data hg node).NodeData.min_pred

  let min_succs_of hg node =
    (get_node_data hg node).NodeData.min_succs

  let succs_of hg node =
    (get_node_data hg node).NodeData.succs

  let preds_of hg node =
    (get_node_data hg node).NodeData.preds

  let update_min_pred node min_pred w hg =
    if Node.is_compound node then 
      get_node_data hg node |> NodeData.update_weight w |> 
      set_node_data hg node 
    else 
      let hg = get_node_data hg node |> NodeData.update_weight w |>
               NodeData.update_min_pred (Some min_pred) |>
               set_node_data hg node
      in
      get_node_data hg min_pred |> NodeData.add_min_succ node |> set_node_data hg min_pred

  let remove_min_pred node hg = 
    if Node.is_compound node then 
      get_node_data hg node |> NodeData.update_weight None |> set_node_data hg node 
    else 
      match min_pred_of hg node with
      | None ->
        hg
      | Some min_pred -> 
        let hg = get_node_data hg node |> 
                 NodeData.update_weight None |> NodeData.update_min_pred None |> set_node_data hg node in
        get_node_data hg min_pred |> NodeData.remove_min_succ node |> set_node_data hg min_pred

  let update_weight node w hg = 
    get_node_data hg node |> NodeData.update_weight w |>
    set_node_data hg node
  let remove_weight node hg = 
    get_node_data hg node |> NodeData.update_weight None |>
    set_node_data hg node
  let is_reachable node hg = get_node_data hg node |> NodeData.is_reachable
  let add_successor node ~succ:succ hg = 
    get_node_data hg node |> NodeData.add_successor succ |>
    (fun x -> if Node.is_compound succ then 
        NodeData.add_min_succ succ x
      else x)
    |>
    set_node_data hg node

  let add_predecessor node ~pred:pred hg = 
    get_node_data hg node |> NodeData.add_predecessor pred |>
    set_node_data hg node
  let remove_successor node ~succ:succ hg = 
    get_node_data hg node |> NodeData.remove_successor succ |>
    set_node_data hg node
  let remove_predecessor node ~pred:pred hg = 
    get_node_data hg node |> NodeData.remove_predecessor pred |>
    set_node_data hg node 

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
    let hg = remove_simple_edge tail head hg in
    if Node.is_compound tail && (succs_of hg tail) = [] then
      remove_compound_node hg tail
      |> IdSet.fold (fun i ->
          remove_successor (Simple i) ~succ:tail
        ) tail_set
    else hg

  (*invariant for the priority queue pq: it only contains simple edges *)
  let scan_successors_after_removal hg initial_pq =
    (* select phase *)
    let rec update_successors_after_removal pq c_set hg =
      match PQ.remove_min_opt pq with
      | None -> (hg, c_set)
      | Some (w_y, y, pq) ->
        (* find hyperedge between the predecessors of y with same weight as minPred(y) *)
        let equivalent =
          List.find_opt (fun pred -> Option.equal Int.equal (weight_of_successor hg pred) w_y) (preds_of hg y)
        in
        match equivalent with 
        | None ->
          (* add successors to priority queue if no such edge has been found *)
          let pq, hg, successors = NodeSet.fold (fun v (acc_pq,hg, succs) ->
              if Node.is_simple v then
                (* if not (PQ.mem v acc_pq) then  *)
                let w_v = weight_of hg v in
                PQ.add ~prio:w_v v acc_pq, hg, succs
                (* else acc_pq *)
              else
                pq, remove_weight v hg, NodeSet.union succs (min_succs_of hg v)
            ) (min_succs_of hg y) (pq,hg, NodeSet.empty ())
          in
          let pq = NodeSet.fold (fun v' acc_pq ->
              PQ.add ~prio:(weight_of hg v') v' acc_pq (*TODO if there is a loop maybe some things are added multiple times to the priority queue*)
            ) successors pq in
          (* set weight to infinite if no such edge has been found *)
          hg |> remove_min_pred y
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
        let hg, _new_weight = recalculate_weight hg v in
        let succs = succs_of hg v in
        let (hg, s') =
          List.fold_left (fun (acc_hg, acc_s) e ->
              let acc_hg, weight_e = recalculate_weight acc_hg e in
              if Node.is_simple e then 
                (acc_hg, PQ.decrease_key ~prio:weight_e e acc_s)
              else
                (* composite node: update also the head of the edge *)
                List.fold_left (fun (acc_hg, acc_s) v' ->
                    let acc_hg, rank_v' = recalculate_weight acc_hg v' in
                    (* update priority of v' in S *)
                    let acc_s = PQ.decrease_key ~prio:rank_v' v' acc_s
                    in
                    acc_hg, acc_s
                  ) (acc_hg, acc_s) (succs_of acc_hg e)
            ) (hg, s') succs
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

  let weight_of a b = Option.map string_of_int (weight_of a b)
end



include Hypergraph
