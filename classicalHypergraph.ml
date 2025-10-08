open Signatures
open Util

(*TODO in this implementation the information pred(c) for a compound node c is redundant as it is already in the set c. We should use an id for the compound nodes instead or remove pred(c)*)
exception UnknownNodeId
exception LogicalError

module Hypergraph : HornClauses = struct
  type t = {
    nodes : node_data NodeMap.t; 
    vars : Vars.t;
  }

  module PQ = PriorityQueue(Node)

  let init ~nr_vars : t =
    { nodes = NodeMap.empty;
      vars = Vars.init ~nr_vars;
    }

  let get_node_data hg node =
    match NodeMap.find_opt node hg.nodes with
    | Some nd -> nd
    | None -> raise UnknownNodeId

  let set_node_data hg node nd =
    { hg with nodes = NodeMap.add node nd hg.nodes }

  let weight_of hg node =
    (get_node_data hg node).weight

  let min_pred_of hg node =
    (get_node_data hg node).min_pred

  let succs_of hg node =
    (get_node_data hg node).succs

  let preds_of hg node =
    (get_node_data hg node).preds

  let get_id hg var =
    let (vars', id) = Vars.var_to_int hg.vars var in
    ({ hg with vars = vars' }, id)

  let simple_node_of_id id = Node.Simple id
  let compound_node_of_intset s = Node.Compound s

  (* The weight the next node would have if there was a simple edge from this node to the successor*)
  let weight_of_successor hg node =
    if Node.is_simple node then
      Option.map ((+) 1) (weight_of hg node)
    else 
      weight_of hg node

  let opt_min x y = Option.fold ~none:x ~some:(fun y -> Option.fold ~none:(Some y) ~some:(fun x -> Some (min y x)) x) y

  let opt_max x y = Option.fold ~none:None ~some:(fun y -> Option.map (max y) x) y

  (* ---------- Add edges ---------- *)


  (* this function updates the weight of the node q2 after an insertion of the edge q1 -> q2.
     It returns true if the weight of q2 was changed. *)
  let update_weight hg q2 q1 =
    let nd2 = get_node_data hg q2 in
    let w1_successor = weight_of_successor hg q1 in
    let new_w =
      if nd2.is_compound then
        opt_max nd2.weight w1_successor
      else
        (* Q2 is a simple node *)
        opt_min nd2.weight w1_successor
    in
    if new_w <> nd2.weight then
      let nd2' = { nd2 with weight = new_w; min_pred = Some q1 } in
      (set_node_data hg q2 nd2', true)
    else (hg, false)

  let add_simple_edge hg q1 q2 =
    let nd1 = get_node_data hg q1 in
    let nd2 = get_node_data hg q2 in
    let nd1' = { nd1 with succs = NodeSet.add q2 nd1.succs } in
    let nd2' = { nd2 with preds = NodeSet.add q1 nd2.preds } in
    let hg = set_node_data (set_node_data hg q1 nd1') q2 nd2' in
    update_weight hg q2 q1

  let add_hyperedge hg xset q_id =
    let q_node = simple_node_of_id q_id in
    if IdSet.mem q_id xset then 
      (*reflexive edges are not allowed*)
      (hg, q_node, false) 
    else if IdSet.cardinal xset = 1 then
      (*add a simple edge between the two nodes*)
      let i = IdSet.choose xset in
      let s = simple_node_of_id i in
      let (hg, changed) = add_simple_edge hg s q_node in
      (hg, q_node, changed)
    else
      let comp = compound_node_of_intset xset in
      let hg =
        IdSet.fold (fun i acc_hg ->
            let s = simple_node_of_id i in
            let (hg2, _) = add_simple_edge acc_hg s comp in
            hg2
          ) xset hg
      in
      let (hg, changed) = add_simple_edge hg comp q_node in
      (hg, comp, changed)

  let closure hg start_node =
    let rec propagate hg worklist =
      match PQ.remove_min_opt worklist with
      | None -> hg
      | Some (_, cur, worklist) ->
        let succs = succs_of hg cur in
        let (hg, new_worklist) =
          NodeSet.fold (fun succ (acc_hg, acc_wl) ->
              let (acc_hg, changed) = update_weight acc_hg succ cur in
              if changed then (acc_hg, PQ.add ~prio:(weight_of hg succ) succ acc_wl) else (acc_hg, acc_wl) (*TODO maybe return the new weight from the function*)
            ) succs (hg, worklist)
        in
        propagate hg new_worklist
    in
    let w = weight_of hg start_node in
    propagate hg (PQ.add ~prio:w start_node PQ.empty)

  (* ---------- Remove edges ---------- *)

  let remove_simple_edge hg q1 q2 =
    let nd1 = get_node_data hg q1 in
    let nd2 = get_node_data hg q2 in
    let nd1' = { nd1 with succs = NodeSet.remove q2 nd1.succs } in
    let nd2' = { nd2 with preds = NodeSet.remove q1 nd2.preds } in
    let nodes' =
      let m1 = NodeMap.add q1 nd1' hg.nodes in
      NodeMap.add q2 nd2' m1
    in
    { hg with nodes = nodes' }

  (* Assumption: xset is not empty*)
  let remove_edge hg xset y_id =
    let y = simple_node_of_id y_id in
    (*TODO check that the edge exists*)
    if IdSet.cardinal xset > 1 then
      let comp = compound_node_of_intset xset in
      let hg =
        IdSet.fold (fun i hg_acc ->
            let s = simple_node_of_id i in
            remove_simple_edge hg_acc s comp
          ) xset hg
      in
      let hg = remove_simple_edge hg comp y in
      if NodeSet.is_empty (succs_of hg comp) then 
        let nodes' = NodeMap.remove comp hg.nodes in
        { hg with nodes = nodes' }
      else hg
    else
      let x = IdSet.choose xset in
      let s = simple_node_of_id x in
      remove_simple_edge hg s y

  let recalculate_weight hg q2 = (*TODO there is a better method for this*)
    let preds = preds_of hg q2 in
    NodeSet.fold (fun q1 acc_hg ->
        let (acc_hg, _ch) = update_weight acc_hg q2 q1 in
        acc_hg
      ) preds hg


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
                    let w_v = weight_of hg v in
                    PQ.add ~prio:w_v v acc_pq
                  | _ -> acc_pq
                end
              else
                NodeSet.fold (fun v' acc_pq ->
                    begin match min_pred_of hg v' with
                      | Some pred when Node.compare pred v = 0 ->
                        PQ.add ~prio:(weight_of hg v') v' acc_pq
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
          let node_y = get_node_data hg y in
          let node_y' = { node_y with min_pred = None; weight = None } in
          let hg = set_node_data hg y node_y' in
          let hg =
            NodeSet.fold (fun e'' acc_hg ->
                let node_e = get_node_data acc_hg e'' in
                let node_e' = { node_e with weight = None } in
                set_node_data acc_hg e'' node_e'
              ) (succs_of hg y) hg
          in
          update_successors_after_removal hg pq (NodeSet.add y c_set)
        | Some edge ->
          (* else set the edge as the new predecessor in the minimal path *)
          let node_y = get_node_data hg y in
          let node_y' = { node_y with min_pred = Some edge; weight = weight_of_successor hg edge} in
          let hg = set_node_data hg y node_y' in
          update_successors_after_removal hg pq c_set   
    in
    let (hg_after_reset, c_set) = update_successors_after_removal hg initial_pq NodeSet.empty in


    (* propagate phase *)
    (* build the priority queue of nodes we need to process *) (*TODO can't you build it instead of building C?*)
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
      | Some (_rank, v, s') ->
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

  (* remove a hyperedge from the graph *)
  let remove hg xset yid =
    let hg = remove_edge hg xset yid in
    let y = simple_node_of_id yid in
    match min_pred_of hg y with
    | Some pred ->
      let pred_matches =
        match pred with
        | Node.Compound s -> IdSet.equal s xset
        | Node.Simple i -> (IdSet.cardinal xset = 1 && IdSet.choose xset = i)
      in
      if pred_matches then
        let w_y = weight_of hg y in
        let pq0 = PQ.add ~prio:w_y y PQ.empty
        in
        scan_successors_after_removal hg pq0
      else hg
    | None -> hg

  type clause = Vars.var * Vars.var list

  (* add an edge body_list -> head to the hypergraph *)
  let add_clause (head, body) hg0 =
    let (hg, head_id) = get_id hg0 head in
    let (hg, body_set) =
      List.fold_left (fun (acc_hg, acc_ids) v ->
          let (acc_hg, id) = get_id acc_hg v in
          (acc_hg, IdSet.add id acc_ids)
        ) (hg, IdSet.empty) body
    in
    let (hg', node, changed) = add_hyperedge hg body_set head_id in
    if changed then 
      closure hg' node
    else hg

  let add_clauses cls hg =
    List.fold_left (fun acc c -> add_clause c acc) hg cls

  let remove_clause (head, body) hg0 =
    let (hg, head_id) = get_id hg0 head in
    let (hg, body_set) =
      List.fold_left (fun (acc_hg, acc_ids) v ->
          let (acc_hg, id) = get_id acc_hg v in
          (acc_hg, IdSet.add id acc_ids)
        ) (hg, IdSet.empty) body
    in
    remove hg body_set head_id

  let reachable_vars hg =
    (* a node is reachable if weight != None *)
    StringMap.bindings hg.vars.var_name_to_int
    |> List.fold_left (fun acc (name, id) ->
        match weight_of hg (simple_node_of_id id) with
        | None -> acc
        | Some _ -> name :: acc
      ) []
    |> List.rev

  let is_reachable var hg =
    match StringMap.find_opt var hg.vars.var_name_to_int with
    | None -> false
    | Some id ->
      match weight_of hg (simple_node_of_id id) with
      | None -> false
      | Some _ -> true

  (*TODO compile with `dot -Tpng graph.dot -o graph.png`*)
  let to_dot (hg : t) : string =
    let get_id_of_node (node: Node.t) =
      match node with 
      | Simple id -> "s" ^ string_of_int id
      | Compound id_set -> "c" ^ string_of_int (Hashtbl.hash id_set) (*TODO give the compound nodes an actual ID*)
    in
    let b = Buffer.create 1024 in
    Buffer.add_string b "digraph hypergraph {\n";
    Buffer.add_string b "  rankdir=LR;\n";
    Buffer.add_string b "  node [fontsize=10, penwidth=1.2];\n";
    (* Print nodes *)
    NodeMap.iter (fun node _ ->
        match node with
        | Simple id ->
          let vname = Vars.int_to_var hg.vars id in
          Buffer.add_string b
            (Printf.sprintf "  n%s [label=\"%s\", shape=point];\n"
               (get_id_of_node node) vname)
        | Compound _ -> 
          let id = get_id_of_node node in 
          Buffer.add_string b
            (Printf.sprintf "  n%s [label=\"%s\", shape=circle, style=filled, fillcolor=white, color=black, width=0.1];\n"
               id id)
      ) hg.nodes;
    (* Print edges *)
    NodeMap.iter (fun node node_data ->
        NodeSet.iter (fun successor ->
            let edge_style =
              match node, successor with
              | Simple _, Compound _ -> " [style=dashed]"
              | _ -> ""
            in
            Buffer.add_string b
              (Printf.sprintf "  n%s -> n%s%s;\n" (get_id_of_node node) (get_id_of_node successor) edge_style)
          ) node_data.succs
      ) hg.nodes;

    Buffer.add_string b "}\n";
    Buffer.contents b

end



include Hypergraph