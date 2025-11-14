open Hypergraph_sig

module SemiParametricHypergraph = struct
  include ParametricHypergraph

  let init ~nr_vars true_node = 
    let t = init ~nr_vars true_node in
    {t with 
     is_parametrized = (fun _ -> false);
     add_parameter_for_new_edges = true;
     is_semi_parametrized = true;
    }

  let is_reachable node t = 
    let mvbdu = ReachabilityStatus.get node t.reachability_status in 
    match mvbdu with 
    | Edges_intbdu.Bool b -> b
    | Edges_intbdu.Mvbdu _ ->
      let valuation_list = 
        EdgeSet.fold 
          (fun edge valuations -> 
             let _, i, _ = get_int_of_edge edge t in
             (i, 
              if Res.Array.get t.current_parameter_valuation i then 1 
              else 0)
             ::valuations) (*TODO maybe we need to reverse the list*)
          t.parametrized_edges []
      in
      let valuation_list = List.rev valuation_list in
      let valuation_list = Edges_intbdu.build_association_list valuation_list in
      let mvbdu_valuation = Edges_intbdu.mvbdu_of_hconsed_asso valuation_list in
      let mvbdu_valuation = Edges_intbdu.mvbdu_normalize mvbdu_valuation t.restriction_bdu in
      let mvbdu_inter = Edges_intbdu.mvbdu_and mvbdu mvbdu_valuation in
      let mvbdu_false = Edges_intbdu.mvbdu_false () in
      not (mvbdu_equal t mvbdu_inter mvbdu_false)

  let add_clauses =
    Hypergraphs.add_clauses add_clause

  let abstract_parameters t mvbdu mvbdu_valuation parameters = 
    let mvbdu_inter = Edges_intbdu.mvbdu_and mvbdu mvbdu_valuation in
    match mvbdu_inter with
    | Bool _ -> mvbdu_inter 
    | Mvbdu mvbdu_inter ->
      let mvbdu = Edges_intbdu.mvbdu_project_abstract_away mvbdu_inter parameters in
      Edges_intbdu.mvbdu_normalize mvbdu t.restriction_bdu

  let remove_parameters_for_all_nodes valuation_list parameters t =
    let valuation_list = List.rev valuation_list in
    let parameters = List.rev parameters in 
    let valuation_list = Edges_intbdu.build_association_list valuation_list in
    let vars = Edges_intbdu.build_variables_list parameters in
    let mvbdu_valuation = Edges_intbdu.mvbdu_of_hconsed_asso valuation_list in
    let mvbdu_valuation = Edges_intbdu.mvbdu_normalize mvbdu_valuation t.restriction_bdu in
    let reachability_status = 
      ReachabilityStatus.map (fun _ mvbdu ->
          abstract_parameters t mvbdu mvbdu_valuation vars
        ) t.reachability_status
    in
    {t with reachability_status}

    (** All the parameters are removed from all edges, all the edges that are deactivated are removed from the graph. After this function was invoked, the next edges that are added will not be parametrized. If you want the new edges to be parametrized, call "open ()".*)
  let close_chapter t =  
    let reachability_status = 
      ReachabilityStatus.map (fun node _ ->
          if is_reachable node t then
            Edges_intbdu.mvbdu_true ()
          else
            Edges_intbdu.mvbdu_false ()
        ) t.reachability_status in
    {t with reachability_status; 
            parametrized_edges = EdgeSet.empty; 
            restriction_bdu = Edges_intbdu.mvbdu_true (); 
            is_parametrized = (fun _ -> false);
            current_parameter_valuation = Res.Array.empty ();
            edge_to_int = EdgeIntConversion.empty;
            add_parameter_for_new_edges = false}

  let and_reachability node mvbdu t = 
    let previous_mvbdu = 
      ReachabilityStatus.get node t.reachability_status in 
    let and_mvbdu = Edges_intbdu.mvbdu_and previous_mvbdu mvbdu in
    let () = ReachabilityStatus.add node and_mvbdu t.reachability_status
    in t

  let recalculate_mvbdu_compound_node_in_open (node, head) t =
    let current_node_mvbdu = reachability_mvbdu_of node t 
    in
    and_reachability head current_node_mvbdu t

  let recalculate_mvbdu_simple_node_in_open head t = 
    let reach = NodeSet.fold (fun node reach ->
        let status = 
          reachability_mvbdu_of node t in
        let t, status = 
          if t.is_parametrized (node, head) then 
            let t, id, _ = get_int_of_edge (node, head) t in  
            let association_list = [(id, 1)] in
            let new_mvbdu_node2 = Edges_intbdu.mvbdu_of_association_list association_list in
            let new_mvbdu_node2 = Edges_intbdu.Mvbdu new_mvbdu_node2 in
            t, Edges_intbdu.mvbdu_and status new_mvbdu_node2 
          else
            t, status 
        in mvbdu_or t reach status
      ) (preds_of t head) (Edges_intbdu.mvbdu_false ()) in
    let () = 
      ReachabilityStatus.add head reach t.reachability_status in
    t

  (*BFS*)
  let closure_for_adding_parameter node t =
    let get_edge_set node t = 
      NodeSet.fold (fun succ edges -> 
          let edge = if Node.is_compound succ then (node, succ) else
              (Node.Compound 0, succ) in
          EdgeSet.add edge edges) (succs_of t node) (EdgeSet.empty) in
    let rec process nodes t = 
      if EdgeSet.is_empty nodes then t 
      else
        let pred, node = EdgeSet.choose nodes in
        let nodes = EdgeSet.remove (pred, node) nodes in
        let t, nodes =
          let old_mvbdu = reachability_mvbdu_of node t in
          let t = if Node.is_compound node then 
              recalculate_mvbdu_compound_node_in_open (pred, node) t
            else 
              (*look at all the predecessors*)
              recalculate_mvbdu_simple_node_in_open node t
          in
          let new_mvbdu = reachability_mvbdu_of node t in
          if mvbdu_equal t old_mvbdu new_mvbdu then
            t, nodes 
          else
            t, EdgeSet.union (get_edge_set node t) nodes
        in process nodes t
    in 
    process (get_edge_set node t) t

  let open_chapter edge_list t = 
    let parametrized_edges, restriction_bdu, t =
      List.fold_left 
        (fun (edges, restriction_bdu, t) (head, tail, _) ->
           let edge = tail, head in 
           let edges = EdgeSet.add edge edges in
           let t, id, _ = get_int_of_edge edge t in
           let range_mvbdu = Edges_intbdu.mvbdu_of_range_list ([id, (Some 0, Some 1)]) in 
           let range_mvbdu = Edges_intbdu.Mvbdu range_mvbdu in
           let restriction_bdu = Edges_intbdu.mvbdu_and range_mvbdu restriction_bdu in
           edges, restriction_bdu, t) 
        (EdgeSet.empty, Edges_intbdu.mvbdu_true (), t)
        edge_list
    in
    let to_unparam = EdgeSet.diff t.parametrized_edges parametrized_edges in
    let to_param = EdgeSet.diff parametrized_edges t.parametrized_edges in
    let is_parametrized = (fun edge -> EdgeSet.mem edge parametrized_edges) in
    let t = { t with parametrized_edges; is_parametrized ; restriction_bdu; add_parameter_for_new_edges = true } in
    let valuation_list, vars = 
      EdgeSet.fold
        (fun (tail, head) (valuation_list, vars) -> 
           let _, i, is_new = get_int_of_edge (tail, head) t in
           if is_new then (valuation_list, vars)(*this should never happen*)
           else
             (i, if Res.Array.get t.current_parameter_valuation i then 1 else 0)::valuation_list, i::vars) 
        to_unparam ([],[]) 
    in
    let t = if valuation_list != [] then
        remove_parameters_for_all_nodes valuation_list vars t 
      else t
    in
    EdgeSet.fold
      (fun (tail, head) t ->
         let old_mvbdu = reachability_mvbdu_of head t in
         let t, id, _ = get_int_of_edge (tail, head) t in
         let t = add_parameter_valuation id true t in
         let t = recalculate_mvbdu_simple_node_in_open head t in
         let new_mvbdu = reachability_mvbdu_of head t in
         if mvbdu_equal t old_mvbdu new_mvbdu then
           t
         else 
           closure_for_adding_parameter head t 
      )
      to_param
      t
end

include SemiParametricHypergraph
