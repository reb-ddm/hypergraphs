open Hypergraph_sig
open Kappa_mvbdu.Mvbdu_wrapper

module Edges_bdu :
  Mvbdu
  with type key = Id.t
   and type value = int
  with type mvbdu = Mvbdu.mvbdu =
  Mvbdu

module Edges_intbdu = Internalize (Edges_bdu)

module ParametricHypergraph  = struct

  module AdjacencyList = MutableNodeMap(IdSet)
  module ReachabilityStatus = MutableNodeMap(struct 
      include Edges_intbdu 
      type t = mvbdu 
      let empty = mvbdu_false
    end)

  type t = {
    adjacency_list : AdjacencyList.t; (* The successors of simple edges are compound edges and vice versa*)
    reachability_status : ReachabilityStatus.t;
    restriction_bdu : Edges_intbdu.mvbdu;
    preds : AdjacencyList.t; (* incoming edges *)
  }

  let init ~nr_vars true_node = 
    let parameters = Remanent_parameters.get_parameters ~html_mode:false ~called_from:Remanent_parameters_sig.Internalised () in
    let () = Edges_intbdu.init parameters in
    let reachability_status = ReachabilityStatus.empty nr_vars in
    let () = ReachabilityStatus.add true_node (Edges_intbdu.mvbdu_true ()) reachability_status in
    {
      adjacency_list = AdjacencyList.empty nr_vars;
      reachability_status;
      restriction_bdu = Edges_intbdu.mvbdu_true ();
      preds = AdjacencyList.empty nr_vars;
    }

  let is_reachable node t = 
    let mvbdu = ReachabilityStatus.get node t.reachability_status in 
    let mvbdu_false = Edges_intbdu.mvbdu_false () in
    not (Edges_intbdu.equal mvbdu mvbdu_false)

  let mvbdu_and = Edges_intbdu.mvbdu_and

  let mvbdu_or t mvbdu1 mvbdu2 = 
    let or_mvbdu = Edges_intbdu.mvbdu_or mvbdu1 mvbdu2 in
    Edges_intbdu.mvbdu_and or_mvbdu t.restriction_bdu

  let or_reachability node mvbdu t = 
    let previous_mvbdu = 
      ReachabilityStatus.get node t.reachability_status in 
    let or_mvbdu = mvbdu_or t previous_mvbdu mvbdu in
    let () = ReachabilityStatus.add node or_mvbdu t.reachability_status in
    t

  let abstract_away_parameter mvbdu parameter = 
    let vars = Edges_intbdu.build_variables_list [parameter] in
    Edges_intbdu.mvbdu_project_abstract_away mvbdu vars

  let succs_of t node =
    AdjacencyList.get node t.adjacency_list

  let preds_of t node =
    AdjacencyList.get node t.preds

  let reachability_mvbdu_of node t =
    ReachabilityStatus.get node t.reachability_status

  let set_successors node succs t = 
    let () = 
      AdjacencyList.add node succs t.adjacency_list in
    t

  let add_successor node ~succ t = 
    let successors = succs_of t node in 
    if IdSet.mem succ successors then 
      t
    else
      let new_set = IdSet.add succ successors in
      set_successors node new_set t

  let set_predecessors node p t = 
    let () = 
      AdjacencyList.add node p t.preds in
    t

  let add_predecessor node ~pred t = 
    let predecessors = preds_of t node in 
    if IdSet.mem pred predecessors then 
      t
    else
      let new_set = IdSet.add pred predecessors in
      set_predecessors node new_set t

  let recalculate_mvbdu_compound id set t = 
    let association_list = [(id, 1)] in
    let new_mvbdu = Edges_intbdu.mvbdu_of_association_list association_list in
    let reach = IdSet.fold (fun node reach ->
        let status = 
          reachability_mvbdu_of (Simple node) t
        in mvbdu_and reach status
      ) set new_mvbdu in
    let () = 
      ReachabilityStatus.add (Compound id) reach t.reachability_status
    in t

  let add_compound_node node set t =
    match node with
    | Node.Compound id ->
      let t = recalculate_mvbdu_compound id set t
      in
      let range_mvbdu = Edges_intbdu.mvbdu_of_range_list ([id, (Some 0, Some 1)]) in (*TODO is the range inclusive or not?*)
      let restriction_bdu = mvbdu_and range_mvbdu t.restriction_bdu in
      {t with restriction_bdu}
    | Node.Simple _ -> t

  let remove_compound_node t id = 
    let () =  AdjacencyList.remove (Node.Compound id) t.adjacency_list in
    let () = ReachabilityStatus.remove (Node.Compound id) t.reachability_status in 
    {t with 
     restriction_bdu = abstract_away_parameter t.restriction_bdu id}

  (* ---------- Add edges ---------- *)
  let add_hyperedge t tail_set tail head =
    if Node.is_reflexive_edge tail_set head then 
      (*reflexive edges are not allowed*)
      t
    else 
      match tail, head with 
      | Node.Compound node_id, Node.Simple head_id ->
        let t = 
          (* add nodes from the simple edges to the compound edge "tail" *)
          IdSet.fold (
            fun i acc_hg -> add_successor (Simple i) ~succ:node_id acc_hg
          ) tail_set (add_compound_node tail tail_set t) 
        in
        let t = set_predecessors tail tail_set t in
        let t = add_successor tail ~succ:head_id t in
        let t = add_predecessor head ~pred:node_id t in
        or_reachability head (reachability_mvbdu_of tail t) t 
      | Node.Compound _,_-> print_endline "ERROR: EDGE WAS NOT ADDED Compound"; t
      | Node.Simple _,_ -> print_endline "ERROR: EDGE WAS NOT ADDED Simple"; t

  let rec closure node t =
    let node_reachability = 
      reachability_mvbdu_of node t in
    IdSet.fold (fun succ t ->
        if Node.is_simple node then (*therefore the successor is a compound node*)
          (*look at all the predecessors*)
          let old_mvbdu = reachability_mvbdu_of (Compound succ) t in
          let t = recalculate_mvbdu_compound succ (preds_of t (Compound succ)) t in
          let new_mvbdu = reachability_mvbdu_of (Compound succ) t in
          if Edges_intbdu.equal old_mvbdu new_mvbdu then
            t else
            closure (Compound succ) t
        else 
          let old_mvbdu = reachability_mvbdu_of (Simple succ) t in
          let t = or_reachability (Simple succ) node_reachability t in
          let new_mvbdu = reachability_mvbdu_of (Simple succ) t in
          if Edges_intbdu.equal old_mvbdu new_mvbdu then
            t else
            closure (Compound succ) t
      ) (succs_of t node) t

  (* add an edge x -> y to the hypergraph *)
  let add_clause (y, x, x_set) t =
    let old_mvbdu = reachability_mvbdu_of y t in
    let t = add_hyperedge t x_set x y in
    let new_mvbdu = reachability_mvbdu_of y t in
    if Edges_intbdu.equal old_mvbdu new_mvbdu then
      t
    else closure y t 

  let add_clauses cls hg =
    List.fold_left (fun acc c -> add_clause c acc) hg cls

  let weight_of _t _node =  
    None

  let remove_successor node ~succ t = 
    let successors = succs_of t node in 
    if IdSet.mem succ successors then 
      let new_set = IdSet.remove succ successors in
      let () = 
        AdjacencyList.add node new_set t.adjacency_list in
      t
    else
      t

  let remove_predecessor node ~pred t = 
    let predecessors = preds_of t node in 
    if IdSet.mem pred predecessors then 
      let new_set = IdSet.remove pred predecessors in
      let () = 
        AdjacencyList.add node new_set t.adjacency_list in
      t
    else
      t

  let remove_hyperedge tail_set tail head hg =
    let hg = remove_successor (Node.Compound tail) ~succ:head hg in
    let hg = remove_predecessor (Node.Simple head) ~pred:tail hg in
    if IdSet.is_empty (succs_of hg (Node.Compound tail)) then
      let hg = 
        IdSet.fold (fun i ->
            remove_successor (Simple i) ~succ:tail
          ) tail_set hg
      in
      remove_compound_node hg tail
    else hg

  let remove_abstract_parameter_for_all_nodes p t =
    let p_false_mvbdu = Edges_intbdu.mvbdu_of_association_list [(p, 0)] in
    let reachability_status = 
      ReachabilityStatus.map (fun _ mvbdu ->
          let mvbdu = mvbdu_and p_false_mvbdu mvbdu in
          abstract_away_parameter mvbdu p
        ) t.reachability_status
    in 
    {t with reachability_status}

  (** Recalculates all weights from scratch. *)
  let remove_clause (y, x, x_set) hg = 
    if Node.is_reflexive_edge x_set y then 
      (*reflexive edges are not allowed*)
      hg 
    else
      match x,y with 
      | Node.Compound x_id, Node.Simple y_id -> 
        hg |> remove_hyperedge x_set x_id y_id 
        |> remove_abstract_parameter_for_all_nodes x_id
      | Node.Compound _, _ | Node.Simple _,_ -> hg

  let succs_of t node = 
    let succs = succs_of t node in
    IdSet.fold (fun succ succs -> 
        let succ = if Node.is_compound node then Node.Simple succ else Node.Compound succ in
        NodeSet.add succ succs
      ) succs (NodeSet.empty ())

end

include ParametricHypergraph
