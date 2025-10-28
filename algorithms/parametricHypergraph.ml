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

  module AdjacencyList = MutableNodeMap(NodeSet)
  module ReachabilityStatus = MutableNodeMap(struct 
      include Edges_intbdu 
      type t = mvbdu 
      let empty = mvbdu_false
    end)

  type t = {
    adjacency_list : AdjacencyList.t; 
    reachability_status : ReachabilityStatus.t;
    restriction_bdu : Edges_intbdu.mvbdu;
    preds : AdjacencyList.t; (* incoming edges *)
    edge_to_int : EdgeIntConversion.t;
  }

  let _print_mvbdu mvbdu =
    let parameters = Remanent_parameters.get_parameters ~html_mode:false ~called_from:Remanent_parameters_sig.Internalised () in
    Edges_intbdu.print parameters mvbdu

  let init ~nr_vars true_node = 
    let parameters = Remanent_parameters.get_parameters ~html_mode:false ~called_from:Remanent_parameters_sig.Internalised () in
    let () = if not (Edges_intbdu.is_init ()) then 
        Edges_intbdu.init parameters 
    in
    let reachability_status = ReachabilityStatus.empty nr_vars in
    let () = ReachabilityStatus.add true_node (Edges_intbdu.mvbdu_true ()) reachability_status in
    {
      adjacency_list = AdjacencyList.empty nr_vars;
      reachability_status;
      restriction_bdu = Edges_intbdu.mvbdu_true ();
      preds = AdjacencyList.empty nr_vars;
      edge_to_int = EdgeIntConversion.empty;
    }

  let is_reachable node t = 
    (* let () = print_endline ("is_reachable "^Node.to_string node) in *)
    let mvbdu = ReachabilityStatus.get node t.reachability_status in 
    let mvbdu_false = Edges_intbdu.mvbdu_false () in
    not (Edges_intbdu.equal mvbdu mvbdu_false)

  let get_int_of_edge edge t = 
    let edge_to_int, i = EdgeIntConversion.val_to_int t.edge_to_int edge in
    {t with edge_to_int}, i

  let mvbdu_and = Edges_intbdu.mvbdu_and

  let mvbdu_or t mvbdu1 mvbdu2 = 
    let or_mvbdu = Edges_intbdu.mvbdu_or mvbdu1 mvbdu2 in
    Edges_intbdu.mvbdu_and or_mvbdu t.restriction_bdu

  let or_reachability node mvbdu t = 
    (* let () = print_endline ("or_reachability "^Node.to_string node) in *)
    let previous_mvbdu = 
      ReachabilityStatus.get node t.reachability_status in 
    let or_mvbdu = mvbdu_or t previous_mvbdu mvbdu in
    let () = ReachabilityStatus.add node or_mvbdu t.reachability_status
    in t 

  let abstract_away_edge mvbdu edge t = 
    let t, parameter = get_int_of_edge edge t in
    let vars = Edges_intbdu.build_variables_list [parameter] in
    t, Edges_intbdu.mvbdu_project_abstract_away mvbdu vars

  let succs_of t node =
    (* let () = print_endline ("succs_of "^Node.to_string node) in *)
    AdjacencyList.get node t.adjacency_list

  let preds_of t node =
    (* let () = print_endline ("preds_of "^Node.to_string node) in *)
    AdjacencyList.get node t.preds

  let reachability_mvbdu_of node t =
    ReachabilityStatus.get node t.reachability_status

  let set_successors node succs t = 
    let () = 
      AdjacencyList.add node succs t.adjacency_list in
    t

  let add_successor node ~succ t = 
    let successors = succs_of t node in 
    if NodeSet.mem succ successors then 
      t
    else
      let new_set = NodeSet.add succ successors in
      set_successors node new_set t

  let set_predecessors node p t = 
    let () = 
      AdjacencyList.add node p t.preds in
    t

  let add_predecessor node ~pred t = 
    let predecessors = preds_of t node in 
    if NodeSet.mem pred predecessors then 
      t
    else
      let new_set = NodeSet.add pred predecessors in
      set_predecessors node new_set t

  let recalculate_mvbdu_simple_edge (node, head) t =
    let t, id = get_int_of_edge (node, head) t in  
    let association_list = [(id, 1)] in
    let new_mvbdu_node2 = Edges_intbdu.mvbdu_of_association_list association_list in
    let reach_and_edge = mvbdu_and (reachability_mvbdu_of node t) new_mvbdu_node2 in
    or_reachability head reach_and_edge t 

  let recalculate_mvbdu_compound_node node1 set fold to_node t = 
    let reach = fold (fun node reach ->
        let node = to_node node in
        let status = 
          reachability_mvbdu_of node t
        in mvbdu_and reach status
      ) set (Edges_intbdu.mvbdu_true ()) in
    let () = 
      ReachabilityStatus.add node1 reach t.reachability_status in
    t

  let add_new_edge_reachabilities node set head fold to_node t =
    let t =
      if Node.is_simple node || ReachabilityStatus.mem node t.reachability_status then 
        t
      else
        recalculate_mvbdu_compound_node node set fold to_node t
    in
    let t = recalculate_mvbdu_simple_edge (node, head) t in 
    let t, id = get_int_of_edge (node, head) t in  
    let range_mvbdu = Edges_intbdu.mvbdu_of_range_list ([id, (Some 0, Some 1)]) in (*TODO is the range inclusive or not?*)
    let restriction_bdu = mvbdu_and range_mvbdu t.restriction_bdu in
    {t with restriction_bdu}

  let remove_compound_node t (head, tail) = 
    let () = AdjacencyList.remove tail t.adjacency_list in
    let () = ReachabilityStatus.remove tail t.reachability_status in 
    (* let () = print_endline ("abstract_away_edge restriction_bdu") in *)
    let t, restriction_bdu = abstract_away_edge t.restriction_bdu (head, tail) t in
    {t with restriction_bdu;}

  (* ---------- Add edges ---------- *)
  let add_simple_edge q1 q2 hg =
    hg 
    |> add_successor q1 ~succ:q2
    |> add_predecessor q2 ~pred:q1 

  let add_hyperedge t tail_set tail head =
    if Node.is_reflexive_edge tail_set head then 
      (*reflexive edges are not allowed*)
      t
    else 
      let t = add_new_edge_reachabilities tail tail_set head IdSet.fold (fun x -> Simple x) t in
      let t = if Node.is_compound tail then  
          (* add nodes from the simple edges to the compound edge "tail" *)
          let t, node_set = IdSet.fold (
              fun i (t, preds) -> add_successor (Simple i) ~succ:tail t, NodeSet.add (Simple i) preds
            ) tail_set (t, NodeSet.empty ()) 
          in set_predecessors tail node_set t
        else t
      in 
      t |> add_simple_edge tail head 

  let rec closure node t =
    NodeSet.fold (fun succ t ->
        let old_mvbdu = reachability_mvbdu_of succ t in
        let t = if Node.is_compound succ then 
            (*look at all the predecessors*)
            recalculate_mvbdu_compound_node succ (preds_of t succ) NodeSet.fold (fun x -> x) t 
          else recalculate_mvbdu_simple_edge (node, succ) t
        in
        let new_mvbdu = reachability_mvbdu_of succ t in
        if Edges_intbdu.equal old_mvbdu new_mvbdu then
          t else
          closure succ t
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

  let weight_of t node =  
    let mvbdu = reachability_mvbdu_of node t in
    let formula = Edges_intbdu.mvbdu_to_formula ~cartesian_decomposition:false mvbdu in
    let formula = Logical_formulae.simplify formula in
    let _, string_formula = Logical_formulae.convert_p (fun x _ -> (), string_of_int x)  () formula in
    let string = Logical_formulae.print_formula "" (^) string_formula in
    Some string

  let remove_successor node ~succ t = 
    let successors = succs_of t node in 
    if NodeSet.mem succ successors then 
      let new_set = NodeSet.remove succ successors in
      let () = 
        AdjacencyList.add node new_set t.adjacency_list in
      t
    else
      t

  let remove_predecessor node ~pred t = 
    let predecessors = preds_of t node in 
    if NodeSet.mem pred predecessors then 
      let new_set = NodeSet.remove pred predecessors in
      let () = 
        AdjacencyList.add node new_set t.preds in
      t
    else
      t

  let remove_hyperedge tail_set tail head hg =
    let hg = remove_successor tail ~succ:head hg in
    let hg = remove_predecessor head ~pred:tail hg in
    if Node.is_compound tail && NodeSet.is_empty (succs_of hg tail) then (*TODO what if true is removed?*)
      let hg = 
        IdSet.fold (fun i ->
            remove_successor (Simple i) ~succ:tail
          ) tail_set hg
      in
      remove_compound_node hg (head, tail)
    else hg

  let remove_edge_for_all_nodes edge t =
    let t, p = get_int_of_edge edge t in
    let p_false_mvbdu = Edges_intbdu.mvbdu_of_association_list [(p, 0)] in
    let reachability_status = 
      ReachabilityStatus.map (fun _ mvbdu ->
          let mvbdu = mvbdu_and p_false_mvbdu mvbdu in
          (* let () = print_endline ("abstract_away_edge "^Node.to_string node) in *)
          let _, mvbdu = abstract_away_edge mvbdu edge t in mvbdu
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
      |> remove_edge_for_all_nodes (x,y)

end

include ParametricHypergraph
